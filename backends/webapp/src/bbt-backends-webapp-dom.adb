-- -----------------------------------------------------------------------------
-- bbt, the black box tester (https://github.com/LionelDraghi/bbt)
-- SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
-- SPDX-FileCopyrightText: 2026, Lionel Draghi
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with BBT_Backends.Text;

package body BBT.Backends.Webapp.Dom is

   use Ada.Strings.Unbounded;

   Helper : constant String :=
     "var role_of=function(e){"
     & "var r=e.getAttribute('role');"
     & "if(r){return r;}"
     & "var t=e.tagName.toLowerCase();"
     & "switch(t){"
     & "case 'input':return 'textbox';"
     & "case 'li':return 'listitem';"
     & "case 'ul':case 'ol':return 'list';"
     & "case 'a':return 'link';"
     & "case 'h1':case 'h2':case 'h3':case 'h4':case 'h5':case 'h6':"
     & "return 'heading';"
     & "default:return t;}};"
     & "var name_of=function(e){"
     & "var l=e.getAttribute('aria-label');"
     & "if(l!==null){return l;}"
     & "return (e.textContent||'').replace(/\s+/g,' ').trim();};"
     & "var shown=function(e){"
     & "return !e.hasAttribute('hidden')&&e.getClientRects().length>0;};"
     & "var find=function(role,name){"
     & "var all=document.querySelectorAll('*');"
     & "for(var i=0;i<all.length;i++){"
     & "var e=all[i];"
     & "if(shown(e)&&role_of(e)===role&&name_of(e)===name){return e;}}"
     & "return null;};";
   --  The prelude of every script: the role mapping (the ARIA role
   --  taxonomy the vocabulary is grounded on, native elements
   --  included), the accessible name (aria-label, else the rendered
   --  text — the two sources of the witness's naming contract), the
   --  visibility test (a hidden or laid-out-away element is not in the
   --  accessibility tree), and the lookup itself.

   function Script (Code : String) return String is
     ("return (function(){" & Helper & Code & "})();");
   --  The WebDriver script body is a function body: the lookup value is
   --  only returned to the caller by an explicit top-level return.

   function Js_Literal (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String ("""");
   begin
      for C of Text loop
         case C is
            when '"'    => Append (Result, "\""");
            when '\'    => Append (Result, "\\");
            when Ada.Characters.Latin_1.LF =>
               Append (Result, "\n");
            when Ada.Characters.Latin_1.CR =>
               Append (Result, "\r");
            when Ada.Characters.Latin_1.HT =>
               Append (Result, "\t");
            when others => Append (Result, C);
         end case;
      end loop;
      Append (Result, """");
      return To_String (Result);
   end Js_Literal;

   function Lookup (Role : String; Name : String) return String is
     ("find(" & Js_Literal (BBT_Backends.Text.Normalized (Role))
      & "," & Js_Literal (BBT_Backends.Text.Normalized (Name)) & ")");

   function Exists_Script (Role : String; Name : String) return String is
     (Script ("return " & Lookup (Role, Name) & "!==null;"));

   function Mark_Script (Role : String; Name : String) return String is
     (Script
        ("var e=" & Lookup (Role, Name) & ";"
         & "if(e===null){return '';}"
         & "var old=document.querySelectorAll('[data-bbt-target]');"
         & "for(var i=0;i<old.length;i++){"
         & "old[i].removeAttribute('data-bbt-target');}"
         & "e.setAttribute('data-bbt-target','1');"
         & "return 'found';"));

   function Enabled_Script (Role : String; Name : String) return String is
     (Script
        ("var e=" & Lookup (Role, Name) & ";"
         & "if(e===null){return false;}"
         & "if(e.disabled){return false;}"
         & "if(e.getAttribute('aria-disabled')==='true'){return false;}"
         & "return true;"));

   function Text_Script (Role : String; Name : String) return String is
     (Script
        ("var e=" & Lookup (Role, Name) & ";"
         & "if(e===null){return '';}"
         & "return e.innerText||e.textContent||'';"));

   function Child_Count_Script (Role : String; Name : String) return String is
     (Script
        ("var e=" & Lookup (Role, Name) & ";"
         & "if(e===null){return -1;}"
         & "var n=0;"
         & "for(var i=0;i<e.children.length;i++){"
         & "if(shown(e.children[i])){n=n+1;}}"
         & "return n;"));

   function Value_Script (Role : String; Name : String) return String is
     (Script
        ("var e=" & Lookup (Role, Name) & ";"
         & "if(e===null||e.value===undefined){return '';}"
         & "return e.value;"));

   function Set_Value_Script
     (Role : String; Name : String; Value : String) return String
   is
     (Script
        ("var e=" & Lookup (Role, Name) & ";"
         & "if(e===null){return false;}"
         & "e.focus();"
         & "e.value=" & Js_Literal (Value) & ";"
         & "e.dispatchEvent(new Event('input',{bubbles:true}));"
         & "return true;"));

   function Window_Text_Script return String is
     (Script ("return (document.body&&document.body.innerText)||'';"));

   function Ready_Script return String is
     (Script ("return document.readyState==='complete';"));

end BBT.Backends.Webapp.Dom;
