/* bbt debugger page enhancements.
   Loaded by boot.html after jquery and the vendored markdown-it
   bundles (markdown-it.min.js, markdown-it-anchor.min.js).

   The server sends the scenario file as one div per source line
   (class "line", id "ln_<n>", raw escaped text). This script:

   - rebuilds the full source from those divs, renders it with the
     vendored markdown-it (CommonMark, GFM tables and strikethrough,
     GitHub-like heading anchors), and replaces #content with the
     rendered document;
   - tags every rendered block with its source line range
     (data-source-line / data-source-end, from markdown-it's
     token.map), so steps and failures highlight the block holding
     the line, and ToC anchors scroll to the right block;
   - renders mermaid code fences as diagrams, in place;
   - falls back to the raw line divs (plain source, exact line
     highlight) when markdown-it did not load. */

/* -------------------------------------------------------------------------- */
/*  Markdown rendering                                                        */
/* -------------------------------------------------------------------------- */

var line_map = null;
/*  The blocks of the currently displayed file: one entry per element
    carrying a data-source-line attribute, { el, s, e } with s, e the
    inclusive 1-based source line range. Computed after each render. */

var md = null;

var github_slug = function (s) {
   /*  GitHub's heading id scheme: lower case, punctuation dropped
       (dashes and underscores kept), runs of spaces become one dash. */
   return s.trim ().toLowerCase ()
      .replace (/[^\p{L}\p{N}\s\-_]/gu, '')
      .replace (/\s+/g, '-');
};

var build_md = function () {
   if (typeof window.markdownit !== 'function') { return; }
   md = window.markdownit ({ html : true, linkify : true });
   if (typeof window.markdownItAnchor === 'function') {
      md.use (window.markdownItAnchor, { slugify : github_slug });
   }
   /*  Source line tagging: markdown-it's token.map gives the 0-based
       source line range of each block; exported here as 1-based
       inclusive attributes, the same addressing as the ln_<n> ids. */
   md.core.ruler.push ('bbt_source_line', function (state) {
      var tokens = state.tokens;
      for (var i = 0; i < tokens.length; i++) {
         var t = tokens[i];
         if (t.map && t.type !== 'inline' && t.type !== 'html_block') {
            t.attrSet ('data-source-line', String (t.map[0] + 1));
            t.attrSet ('data-source-end', String (t.map[1]));
         }
      }
   });
};

var compute_line_map = function () {
   line_map = [];
   var els = document.querySelectorAll ('#content [data-source-line]');
   for (var i = 0; i < els.length; i++) {
      var s = parseInt (els[i].getAttribute ('data-source-line'), 10);
      var e = parseInt (els[i].getAttribute ('data-source-end'), 10);
      if (!isNaN (s) && !isNaN (e)) {
         /*  Fences are tagged on their <code> child: the <pre> is the
             visual block, it takes the highlight and the centering. */
         var el = els[i];
         if (el.tagName === 'CODE'
             && el.parentElement && el.parentElement.tagName === 'PRE') {
            el = el.parentElement;
         }
         line_map.push ({ el : el, s : s, e : e });
      }
   }
};

var render_content = function () {
   var c = document.getElementById ('content');
   if (!c || !md) { return; }
   var divs = c.querySelectorAll ('div.line');
   if (divs.length === 0) { return; }   // rendered state: raw divs are gone
   /*  No renderer loaded: raw fallback, the line divs stay. */
   if (!md) { return; }
   var lines = [];
   for (var i = 0; i < divs.length; i++) {
      var m = /^ln_(\d+)$/.exec (divs[i].id);
      if (m) { lines[parseInt (m[1], 10)] = divs[i].textContent; }
   }
   var src = '';
   for (var k = 1; k < lines.length; k++) {
      src += (k in lines ? lines[k] : '') + '\n';
   }
   c.innerHTML = md.render (src);
   c.classList.add ('md');
   compute_line_map ();
};

/* -------------------------------------------------------------------------- */
/*  Mermaid fences: the renderer leaves them as language-mermaid code         */
/*  blocks; rendered in place as diagrams (mermaid v9 callback form).         */
/* -------------------------------------------------------------------------- */

var render_mermaid = function () {
   if (typeof window.mermaid === 'undefined') { return; }
   var blocks = document.querySelectorAll ('#content pre > code.language-mermaid');
   for (var i = 0; i < blocks.length; i++) {
      var code = blocks[i];
      var pre = code.parentElement;
      if (pre.hasAttribute ('data-mmd')) { continue; }
      pre.setAttribute ('data-mmd', '1');
      (function (pre, text, id) {
         mermaid.render (id, text, function (svg) {
            pre.innerHTML = svg;
            pre.classList.add ('mm');
         });
      }) (pre, code.textContent, 'mmr' + i);
   }
};

/* -------------------------------------------------------------------------- */
/*  Line addressing: the fallback mode answers with the raw line divs,        */
/*  the rendered mode with the innermost block covering the line.             */
/* -------------------------------------------------------------------------- */

var line_element = function (n) {
   var raw = document.getElementById ('ln_' + n);
   if (raw) { return raw; }
   if (!line_map) { return null; }
   var best = null;
   for (var i = 0; i < line_map.length; i++) {
      var b = line_map[i];
      if (b.s <= n && n <= b.e) {
         if (!best
             || (b.e - b.s) < (best.e - best.s)
             || ((b.e - b.s) === (best.e - best.s) && b.s > best.s)) {
            best = b;
         }
      }
   }
   /*  Lines outside any block (blank lines, trailing content):
       the nearest block at or before the line. */
   if (!best) {
      for (var j = 0; j < line_map.length; j++) {
         var p = line_map[j];
         if (p.s <= n && (!best || p.s > best.s)) { best = p; }
      }
   }
   return best ? best.el : null;
};

var center_line = function (n) {
   var c = document.getElementById ('content');
   var e = line_element (n);
   if (!c || !e) { return; }
   /*  Center e in the content view: scroll the container only, never
       the page (a whole-file jump would lose the debug focus). The
       rects are used rather than offsetTop: rendered blocks may sit
       under positioned ancestors (the line number ::before), which
       breaks the offsetTop relative reference. */
   var er = e.getBoundingClientRect ();
   var cr = c.getBoundingClientRect ();
   c.scrollTop += er.top - cr.top - (c.clientHeight - er.height) / 2;
};

/* Clicks: an in-document anchor scrolls to the block that rendered
   the targeted heading; any other link opens in a new tab, so that a
   documentation click never discards the debug session. */
document.addEventListener ('click', function (event) {
   var a = event.target && event.target.closest ? event.target.closest ('a') : null;
   if (!a) { return; }
   var href = a.getAttribute ('href');
   if (!href) { return; }
   if (href.charAt (0) === '#') {
      event.preventDefault ();
      var target = document.getElementById (href.slice (1));
      if (target && target.hasAttribute ('data-source-line')) {
         center_line (parseInt (target.getAttribute ('data-source-line'), 10));
      } else if (a.hasAttribute ('data-ln')) {
         /* Legacy server-emitted anchors (raw fallback pages). */
         center_line (parseInt (a.getAttribute ('data-ln'), 10));
      }
   } else {
      event.preventDefault ();
      window.open (href, '_blank');
   }
});

/* The tree panel's width is adjustable through the #divider handle.
   Delegated mousedown: the divider is inserted by the server at
   connection time, after this script has run. The drag sets #nav's
   flex basis inline (it overrides the layout default), clamped to
   keep room for the content pane. */
$(document).on ('mousedown', '#divider', function (event) {
   event.preventDefault ();
   var nav = document.getElementById ('nav');
   var move = function (event) {
      if (!nav) { return; }
      var x = event.clientX - nav.getBoundingClientRect ().left;
      x = Math.max (150, Math.min (x, window.innerWidth - 300));
      nav.style.flexBasis = x + 'px';
      nav.style.width = x + 'px';
   };
   var stop = function () {
      document.removeEventListener ('mousemove', move);
      document.removeEventListener ('mouseup', stop);
      document.body.style.userSelect = '';
   };
   document.addEventListener ('mousemove', move);
   document.addEventListener ('mouseup', stop);
   document.body.style.userSelect = 'none';
});

/* The terminal panel's height is adjustable through the #tdivider
   handle, between the mainrow and the terminal. Dragging up grows the
   terminal (it sets #terminal's flex basis inline, overriding the
   25vh layout default), clamped to leave room for the mainrow. */
$(document).on ('mousedown', '#tdivider', function (event) {
   event.preventDefault ();
   var term = document.getElementById ('terminal');
   var move = function (event) {
      if (!term) { return; }
      var h = event.clientY - term.getBoundingClientRect ().top;
      h = Math.max (30, Math.min (h, window.innerHeight / 2));
      term.style.flexBasis = h + 'px';
   };
   var stop = function () {
      document.removeEventListener ('mousemove', move);
      document.removeEventListener ('mouseup', stop);
      document.body.style.userSelect = '';
   };
   document.addEventListener ('mousemove', move);
   document.addEventListener ('mouseup', stop);
   document.body.style.userSelect = 'none';
});

/* Step and failure highlights are requested by the server with a
   'bbt:highlight' custom event on #content: it clears the previous
   highlights, tags the block holding the target line, and keeps it
   in view. In the raw fallback mode, the exact line div is tagged. */
$(document).on ('bbt:highlight', function (event, line, classes) {
   $('#content .hl').removeClass ('hl');
   $('#content .fail').removeClass ('fail');
   var el = line_element (line);
   if (!el) { return; }
   for (var i = 0; i < classes.length; i++) {
      el.classList.add (classes[i]);
   }
   center_line (line);
});

/* Auto expand off: the run position marker (jstree-alias) sits on the
   first visible ancestor of the hidden selected node. The server sets
   window.bbt_pos_node (the run position node id) and
   window.bbt_alias_on at each stop; this routine translates it into
   the visible alias, and the tree's open and close events re-apply
   it, so the cue follows the fold state without waiting for the next
   step. The walk climbs the jstree model, not the DOM: jstree does
   not render a closed branch's children at all, and drops them from
   the DOM on close, so the hidden target may have no element to
   test. */
var pos_alias = function () {
   var inst = $('#navtree').jstree (true);
   if (!inst || !window.bbt_alias_on || !window.bbt_pos_node) { return; }
   $('#navtree .jstree-alias').removeClass ('jstree-alias');
   var v = inst.get_node (window.bbt_pos_node);
   while (v && v.parents.length > 0 && v.parents[0] !== '#') {
      var e = document.getElementById (v.id);
      if (e && e.offsetParent) { break; }
      v = inst.get_node (v.parents[0]);
   }
   var el = v ? document.getElementById (v.id) : null;
   if (el) {
      var a = el.matches ('a.jstree-anchor') ? el
            : el.querySelector ('a.jstree-anchor');
      if (a) { a.classList.add ('jstree-alias'); }
   }
};

$(document).on ('after_close.jstree', '#navtree', pos_alias);
$(document).on ('after_open.jstree', '#navtree', pos_alias);

/* A new MutationObserver pokes each ui batch: raw file payloads are
   rendered, mermaid fences complete at any update. */
var ui_observer = new MutationObserver (function () {
   try { render_content (); }
   catch (e) { console.error (e); }
   try { render_mermaid (); }
   catch (e) { console.error (e); }
});

window.addEventListener ('load', function () {
   build_md ();
   try { mermaid.initialize ({ startOnLoad : false, securityLevel : 'loose' }); }
   catch (e) { /* the run continues without diagrams */ }

   /*  The whole debugger page is built by gnoga after the WebSocket
       connection: #content may not exist yet at load. Observe the
       body subtree instead, and rely on render_content's guard (no
       raw line divs: no-op) to keep the rendered state inert. */
   ui_observer.observe (document.body, { childList : true, subtree : true });

   render_content ();
   render_mermaid ();

   gnoga['render_mermaid'] = render_mermaid;   // cf. page rebuild (reconnect)
   gnoga['render_content'] = render_content;   // idem
   gnoga['pos_alias']      = pos_alias;        // server script, each stop
});
