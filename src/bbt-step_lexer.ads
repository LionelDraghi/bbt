with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT.Documents; use BBT.Documents;

package BBT.Step_Lexer is

   -- --------------------------------------------------------------------------
   function Parse (Line : Unbounded_String) return Step_Details;

end BBT.Step_Lexer;
