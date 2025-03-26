with BBT.IO;

use BBT.IO;

with Ada.Characters,
     Ada.Characters.Handling,
     Ada.Containers.Vectors,
     Ada.Strings.Fixed;

use Ada.Containers;

package body BBT.Tests.Filter_List is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : Location_Type    := No_Location;
                             Verbosity : Verbosity_Levels := Debug;
                             Topic     : Extended_Topics  := IO.Filters)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   -- Define a vector to store the filters
   package Filter_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Filter);

   Filter_List : Filter_Vectors.Vector;

   -- --------------------------------------------------------------------------
   procedure Add_Filter (S : String;
                         A : Apply_On;
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

   -- --------------------------------------------------------------------------
   function Contains (Src : String;
                      Pat : Unbounded_String) return Boolean
   is
      use Ada.Strings.Fixed,
          Ada.Characters.Handling;
   begin
      return Index (Source  => To_Lower (Src),
                    Pattern => To_Lower (+Pat),
                    From    => Src'First) /= 0;
   end Contains;

   -- --------------------------------------------------------------------------
   function Is_Filtered (S : String;
                         I : Filtered_Item) return Boolean is
   begin
      for F of Filter_List loop
         if (F.A = I or F.A = Apply_To_All)
           and then Contains (S, F.S)
           and then F.M = Exclude
         then
            Put_Debug_Line ("Is_Filtered (" & S &
                              ", " & I'Image &
                              ", Mode = " & F.M'Image & ") return True");
            return True;
            -- exit;
            -- The item may be included and excluded several
            -- time, the last to spoke has right.
         end if;
      end loop;
      Put_Debug_Line ("Is_Filtered (" & S &
                        ", " & I'Image & ") return False");
      return False;
   end Is_Filtered;

   -- --------------------------------------------------------------------------
   function Is_Selected (S : String;
                         I : Filtered_Item) return Boolean is
   begin
      for F of Filter_List loop
         if (F.A = I or F.A = Apply_To_All)
           and then Contains (S, F.S)
           and then F.M = Include
         then
            Put_Debug_Line ("Is_Selected (" & S &
                              ", " & I'Image &
                              ", Mode = " & F.M'Image & ") return True");
            return True;
            -- exit;
            -- The item may be included and excluded several
            -- time, the last to spoke has right.
         end if;
      end loop;
      Put_Debug_Line ("Is_Selected (" & S &
                        ", " & I'Image & ") return False");
      return False;
   end Is_Selected;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (S : in out Step_Type) is
      Filtered : constant Boolean := Is_Filtered (+S.Step_String, Step);
      Selected : constant Boolean := not Filtered and
        Is_Selected (+S.Step_String, Step);

   begin
      Put_Debug_Line ("Apply_Filters_To" & (+S.Step_String));
      if Selected then
         Unfilter_Parents (S);
         -- If a Step matches, we also need the enclosing scenario
         Unfilter (S);
         -- if One step is selected, we must mark the parent scenario as
         -- selected, and possibly Background, etc.

      elsif Filtered then
         Documents.Filter (S);

      end if;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (Scen : in out Scenario_Type) is
      Filtered : constant Boolean := Is_Filtered (+Scen.Name, Scenario);
      Selected : constant Boolean := not Filtered and
        Is_Selected (+Scen.Name, Scenario);

   begin
      Put_Debug_Line ("Apply_Filters_To scen " & (+Scen.Name));
      if Filtered then
         Documents.Filter_Tree (Scen);

      elsif Selected then
         Unfilter_Parents (Scen);
         Unfilter_Tree (Scen);
         -- if scenario is selected, we must mark the parent feature or
         -- document as selected, and possibly Background, etc.

      end if;

      for S of Scen.Step_List loop
         Apply_Filters_To (S);
      end loop;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (F : in out Feature_Type) is
      Filtered : constant Boolean := Is_Filtered (+F.Name, Feature);
      Selected : constant Boolean := not Filtered and
        Is_Selected (+F.Name, Feature);

   begin
      Put_Debug_Line ("Apply_Filters_To " & (+F.Name));
      if Filtered then
         Documents.Filter_Tree (F);

      elsif Selected then
         Unfilter_Parents (F);
         Unfilter_Tree (F);

      end if;

      if F.Background /= null then
         Apply_Filters_To (F.Background.all);
      end if;

      for Scen of F.Scenario_List loop
         Apply_Filters_To (Scen);
      end loop;
   end Apply_Filters_To;

   -- --------------------------------------------------------------------------
   procedure Apply_Filters_To (Docs : access Documents_Lists.Vector) is
   begin
      for D of Docs.all loop
         declare
            Filtered : constant Boolean := Is_Filtered (+D.Name, File_Name);
            Selected : constant Boolean := not Filtered and
              Is_Selected (+D.Name, File_Name);

         begin
            Put_Debug_Line ("Apply_Filters_To : """ & (+D.Name) & """");

            if Filtered then
               Documents.Filter_Tree (D);

            elsif Selected then
               Unfilter_Tree (D);

            end if;

            if D.Background /= null then
               Apply_Filters_To (D.Background.all);
            end if;

            for S of D.Scenario_List loop
               Apply_Filters_To (S);
            end loop;

            for F of D.Feature_List loop
               Apply_Filters_To (F);
            end loop;
         end;
      end loop;
   end Apply_Filters_To;

end BBT.Tests.Filter_List;
