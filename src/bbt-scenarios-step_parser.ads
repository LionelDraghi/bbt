with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BBT.Documents;         use BBT.Documents;
with BBT.IO;                use BBT.IO;

package BBT.Scenarios.Step_Parser is

   -- --------------------------------------------------------------------------
   function Parse (Line     :     Unbounded_String;
                   Loc      :     Location_Type;
                   Cmd_List : out Cmd_Lists.Vector) return Step_Type;

   procedure Put_Keywords;
   procedure Put_Grammar;

end BBT.Scenarios.Step_Parser;
