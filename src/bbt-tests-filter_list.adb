with BBT.IO;            use BBT.IO;
with BBT.Tests.Builder; use BBT.Tests.Builder;
with BBT.Documents;     use BBT.Documents;

with Ada.Containers.Vectors; use Ada.Containers;

package body BBT.Tests.Filter_List is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Filters)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   Mode : Global_Mode := Exclusion;

   -- --------------------------------------------------------------------------
   procedure Set_Global_Mode (M : Global_Mode) is
   begin
      Mode := M;
      Put_Debug_Line ("Global Filter Mode set to " & Mode'Image);
   end Set_Global_Mode;

   -- Define a vector to store the filters
   package Filter_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Filter);

   Filter_List : Filter_Vectors.Vector;

   -- --------------------------------------------------------------------------
   procedure Add_Filter (S : String;
                         A : Filters;
                         M : Filter_Mode)
   is
      New_Filter : constant Filter := (S => To_Unbounded_String (S),
                                       A => A,
                                       M => M);
   begin
      Filter_List.Append (New_Filter);
      Put_Debug_Line ("Adding " & M'Image & " filter " & S'Image
                      & " to " & A'Image);
   end Add_Filter;

   Result_List : aliased BBT.Documents.Documents_Lists.Vector;

   -- function Filtered_Document_List return access Documents_Lists.Vector;
   -- --------------------------------------------------------------------------
   function Filtered_Document_List return access Documents_Lists.Vector is
   begin
      -- Iterate over the test list and apply filters
      for Doc of The_Tests_List.all loop
         declare
            Include_Document : Boolean := (Mode = Selection);
         begin
            -- Apply each filter in the Filter_List
            for Filter of Filter_List loop
            --     if Filter.M = Include then
            --        if Filter.A = Doc.Tags and then
            --           Contains (Doc.Tags, Filter.S) then
            --           Include_Document := True;
            --        elsif Filter.A = Names and then
            --           Contains (To_Unbounded_String (Doc.Name), Filter.S) then
            --           Include_Document := True;
            --        end if;
            --     elsif Filter.M = Exclude then
            --        if Filter.A = Tags and then
            --           Contains (Doc.Tags, Filter.S) then
            --           Include_Document := False;
            --        elsif Filter.A = Names and then
            --           Contains (To_Unbounded_String (Doc.Name), Filter.S) then
            --           Include_Document := False;
            --        end if;
            --     end if;
               null;
            end loop;

            -- Add the document to the result list if it matches the filters
            if Include_Document then
               Result_List.Append (Doc);
            end if;
         end;
      end loop;

      return Result_List'Access;
   end Filtered_Document_List;

   --  private
   --     -- type Filter_List is access all Filter_Lists.Vector;
   --     type Filter is record
   --        S : Unbounded_String;
   --        A : Filters;
   --        M : Mode;
   --     end record;
   --
end BBT.Tests.Filter_List;
