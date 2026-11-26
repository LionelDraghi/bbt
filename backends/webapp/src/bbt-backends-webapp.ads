-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

--  Webapp GUI backend: the WebDriver backend, packaged as its own bbt
--  backend crate (cf. docs/dev/design_discussions.md (D16)).
--
--  The application under test is a web application: it serves its page,
--  the backend drives a browser to it through the W3C WebDriver
--  protocol (a chromedriver in the PATH), and the page — its document —
--  plays the role the window plays for a desktop backend (G005). No
--  accessibility bus is involved: the backend computes the vocabulary's
--  role + accessible name lookup on the DOM (cf. BBT.Backends.Webapp.Dom),
--  the way a browser does for the ARIA fallback mapping.
--
--  Correlating the launched application with its window is trivial
--  here, and comes from the configuration rather than from a process
--  walk: the application serves the url read from BBT_WEBAPP_URL
--  (http://127.0.0.1:8080 by default), the backend waits for that url
--  to answer, then navigates the browser to it.
--
--  Closure semantics: closing the window closes the page, not the
--  application, which keeps serving (the spec leaves windowless
--  applications out of the close step's scope, cf. G095). The launched
--  application is reaped by Shutdown, and by the next Launch.

package BBT.Backends.Webapp is

   Backend_Kind : constant String := "webapp";

   type Webapp_Backend is new Backend with null record;
   --  The concrete backend registered by the bbt_webapp main. The
   --  backend state (application pid, driver session, observed title)
   --  lives in the body: the backend operates as one instance,
   --  registered once at startup.

   overriding procedure Launch
     (The_Backend : in out Webapp_Backend;
      Binary_Path : String);
   --  Kills the application left by a previous launch, spawns
   --  Binary_Path, waits for its page to be served, then starts a
   --  browser session on that page. Raises No_Backend_Available when
   --  the application never serves, or no driver/browser is available.

   overriding procedure Shutdown (The_Backend : in out Webapp_Backend);
   --  Kills the launched application, ends the browser session and
   --  reaps the driver

   overriding function Window_Appears
     (The_Backend : Webapp_Backend; Expected_Title : String := "")
      return Boolean;
   --  Polls the page: loaded, and titled Expected_Title when one is
   --  given (G005: the document title is the window name)

   overriding function Observed_Title
     (The_Backend : Webapp_Backend) return String;
   --  The last document title read, for the failure diagnostic

   overriding function Window_Contains
     (The_Backend : Webapp_Backend; Expected : String) return Boolean;
   --  The rendered text of the page, within the harness deadline

   overriding function Widget_Named
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean;
   --  A shown element of role Role and accessible name Name is in the
   --  page (G015)

   overriding function Widget_Enabled
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean;
   --  The user-visible enabled state of the named element: not
   --  disabled, and not aria-disabled

   overriding function Press_Widget
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean;
   --  Semantic activation of the named element: the browser's own
   --  click on it, not injected coordinates (G020)

   overriding function Widget_Contains
     (The_Backend : Webapp_Backend; Role, Name, Expected : String)
      return Boolean;
   --  The rendered text of the named element subtree contains the
   --  expected text (list content checks, G080)

   overriding function Widget_Is_Empty
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Boolean;
   --  The named container holds no shown child

   overriding function Widget_Item_Count
     (The_Backend : Webapp_Backend; Role : String; Name : String)
      return Natural;
   --  The number of shown children of the named container (G080);
   --  0 when the container is absent

   overriding procedure Type_Into
     (The_Backend : in out Webapp_Backend;
      Role        : String;
      Name        : String;
      Text        : String);
   --  Types Text into the named field: its value set, with the input
   --  event a real typing raises, and no validation (G035). Raises
   --  No_Backend_Available when no such field is in the page.

   overriding procedure Enter_Into
     (The_Backend : in out Webapp_Backend;
      Role        : String;
      Name        : String;
      Text        : String);
   --  The Type_Into value set, then the field commit: a synthesized
   --  Return key press on the field (the driver is the tier that
   --  reaches the browser's key handling). Raises No_Backend_Available
   --  when the field is not in the page.

   overriding function Close_Window
     (The_Backend : Webapp_Backend; Title : String := "")
      return Boolean;
   --  Closes the page (the spec's window) through the driver: the
   --  application keeps serving (G095). True when the page is gone,
   --  already gone included.

   Registered : aliased Webapp_Backend;
   --  The singleton backend instance: the bbt_webapp main sets
   --  BBT.Backends.Selected to Registered'Access at startup, before
   --  running the driver

end BBT.Backends.Webapp;
