/* bbt debugger page enhancements: toc links, mermaid fences rendering.
   Loaded by boot.html after jquery. The file is displayed as one div
   per source line (class "line", id "ln_<n>"), server side; this script
   keeps them as-is, and:
   - merges the consecutive divs marked "mm" - the content of a mermaid
     code fence - and renders the diagram in place;
   - scrolls a remote anchor (class "toc", data-ln attribute) into view. */

var mermaid_done = {};
var render_mermaid = function () {
   var blocks = document.querySelectorAll('div.mm');
   for (var i = 0; i < blocks.length; i++) {
      var id = blocks[i].getAttribute('data-mm');
      if (mermaid_done[id] === 1) { continue; }
      var lines = document.querySelectorAll('div.mm[data-mm=\'' + id + '\']');
      var text = '';
      for (var j = 0; j < lines.length; j++) {
         text += lines[j].textContent + '\n';
      }
      /* Defer: mermaid (v9) renders once the diagram elements exist */
      (function (first, rest, text) {      // eslint-disable-line no-loop-func
         mermaid.render ('mmr' + id, text,
            function (svg) {
               first.innerHTML = svg;
               for (var K = 0; K < rest.length; K++) {
                  rest[K].style.display = 'none';
               }
               mermaid_done[id] = 1;
            });
      }) (lines[0], Array.prototype.slice.call (lines, 1), text);
   }
};

var center_line = function (n) {
   var c = document.getElementById ('content');
   var e = document.getElementById ('ln_' + n);
   if (!c || !e) { return; }
   /* Center e in the content view: scroll the container only, never
      the page (a whole-file jump would lose the debug focus). */
   c.scrollTop = e.offsetTop - (c.clientHeight - e.offsetHeight) / 2;
};

document.addEventListener ('click', function (event) {
   var e = event.target;
   if (e.tagName !== 'A' || !e.hasAttribute ('data-ln')) { return; }
   event.preventDefault ();
   center_line (e.getAttribute ('data-ln'));
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

/* Step and failure highlights are requested by the server with a
   'bbt:highlight' custom event on #content: it clears the previous
   highlights, tags the target line, and keeps it in view. */
$(document).on ('bbt:highlight', function (event, line, classes) {
   $('#content .hl').removeClass ('hl');
   $('#content .fail').removeClass ('fail');
   var el = document.getElementById ('ln_' + line);
   if (!el) { return; }
   for (var i = 0; i < classes.length; i++) {
      el.classList.add (classes[i]);
   }
   center_line (line);
});

/* A new MutationObserver pokes each ui batch: mermaid fences may
   complete at any update */
var ui_observer = new MutationObserver (function () {
   try { render_mermaid (); }
   catch (e) { console.error (e); }
});

window.addEventListener ('load', function () {
   try { mermaid.initialize ({ startOnLoad : false, securityLevel : 'loose' }); }
   catch (e) { /* the run continues without diagrams */ }

   var c = document.getElementById ('content');
   if (c) { ui_observer.observe (c, { childList : true, subtree : false }); }

   gnoga['render_mermaid'] = render_mermaid;   // cf. page rebuild (reconnect)
   render_mermaid ();
});
