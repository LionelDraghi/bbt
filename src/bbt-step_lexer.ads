with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT.Documents; use BBT.Documents;

package BBT.Step_Lexer is

   -- --------------------------------------------------------------------------
   function Parse (Line    : Unbounded_String;
                   Context : Extended_Step_Categories) return Step_Details;
   -- Some line are hard to interpret without the context:
   -- line that starts with "And" for example will inherit their type from
   -- the previouly analyzed lines.

end BBT.Step_Lexer;
