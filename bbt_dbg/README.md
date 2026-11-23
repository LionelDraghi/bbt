The web debugger assets (GUI client of BBT.Execution_Control,
served by BBT.GUI through the embedded gnoga server).

- html/boot.html  boot page, based on the gnoga multi_connect
  template, with the debugger layout and styles added. The markdown
  rendering styles (GitHub look) are there too.
- js/boot.js, js/jquery.min.js  gnoga client side message dispatch
  glue, copied unchanged from the gnoga 2.2.3 crate
  (gnoga is GPL3 + GNAT runtime exception; cf. the crate's COPYING
  files in the Alire cache).
- js/mermaid.min.js  the official mermaid v9 browser bundle, vendored
  unchanged from https://github.com/mermaid-js/mermaid (MIT license).
- js/markdown-it.min.js  the official markdown-it 15 browser bundle,
  vendored unchanged from https://github.com/markdown-it/markdown-it
  (MIT license, (c) 2014 Vitaly Puzrin, Alex Kocharin).
- js/markdown-it-anchor.min.js  the official markdown-it-anchor 10
  bundle, vendored unchanged from
  https://github.com/valeriangalliat/markdown-it-anchor (Unlicense).
- js/page.js  the debugger page enhancements: it rebuilds the full
  scenario source from the div-per-line payload sent by BBT.GUI,
  renders it with markdown-it (CommonMark, GFM tables/strikethrough,
  GitHub-like heading anchors), tags the blocks with their source
  line range (data-source-line, from token.map), handles the ToC
  anchor clicks and the step/failure highlights, and renders the
  mermaid fences. It also holds pos_alias, the run position marker
  of a folded tree (auto expand off), applied on each stop and
  re-applied on every tree open or close. When the bundles are not
  reachable, it falls back to the raw line display, highlighted on
  the exact line.

At each --gui run, BBT.GUI deploys copies of these files next to the
bbt binary (paths html/ and js/, gitignored), since gnoga looks for
them relative to the executable location.
