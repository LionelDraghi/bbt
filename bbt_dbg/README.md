The web debugger assets (GUI client of BBT.Execution_Control,
served by BBT.GUI through the embedded gnoga server).

- html/boot.html  boot page, based on the gnoga multi_connect
  template, with the debugger layout and styles added.
- js/boot.js, js/jquery.min.js  gnoga client side message dispatch
  glue, copied unchanged from the gnoga 2.2.3 crate
  (gnoga is GPL3 + GNAT runtime exception; cf. the crate's COPYING
  files in the Alire cache).

At each --gui run, BBT.GUI deploys copies of these files next to the
bbt binary (paths html/ and js/, gitignored), since gnoga looks for
them relative to the executable location.
