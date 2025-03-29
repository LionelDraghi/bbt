
with BBT.Documents,
     BBT.IO,
     Ada.Characters,
     Ada.Characters.Handling,
     Ada.Containers.Vectors,
     Ada.Strings.Fixed;

use BBT,
    BBT.Documents,
    Ada.Containers;

package body BBT.Tests.Filter_List is

   -- --------------------------------------------------------------------------
   procedure Put_Debug_Line (Item      : String;
                             Location  : IO.Location_Type    := IO.No_Location;
                             Verbosity : IO.Verbosity_Levels := IO.Debug;
                             Topic     : IO.Extended_Topics  := IO.Filters)
                             renames BBT.IO.Put_Line;

   -- --------------------------------------------------------------------------
   -- Define a vector to store the filters
   package Filter_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Filter);

   Filter_Chain : Filter_Vectors.Vector;

   -- --------------------------------------------------------------------------
   procedure Add_Filter (Pattern : String;
                         Target : Filter_Scope;
                         Mode : Filter_Mode)
   is
      New_Filter : constant Filter := (Pattern => To_Unbounded_String (Pattern),
                                       Target => Target,
                                       Mode => Mode);
   begin
      Filter_Chain.Append (New_Filter);
      Put_Debug_Line ("Adding " & Mode'Image & " filter " & Pattern'Image
                      & " to " & Target'Image);
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
   function Is_Filtered (S         : String;
                         Item_Type : Apply_On) return Filter_Result is
   begin
      for F of reverse Filter_Chain loop
         -- The item may be included and excluded several time :
         -- looking into the chain in reverse order is an optimized way to
         -- have the bahavior "the last to spoke has right".
         if (F.Target = Item_Type or F.Target = Apply_To_All)
           and then Contains (S, F.Pattern)
         then
            declare
               Msg : constant String :=
                       "Is_Filtered : " & Item_Type'Image & " '" & S
                       & "' matches filter '" & Short_Image (F) & "' -> ";
            begin
               case F.Mode is
               when Exclude =>
                  Put_Debug_Line (Msg & "Filtered");
                  return Filtered;
               when Include =>
                  Put_Debug_Line (Msg & "Selected");
                  return Selected;
               end case;
            end;
         end if;
      end loop;
      --  Put_Debug_Line ("Is_Filtered (" & S &
      --                    ", " & Item_Type'Image & ") return Mo_Match");
      return No_Match;
   end Is_Filtered;

   --  -- --------------------------------------------------------------------------
   --  function Is_Filtered (S : String;
   --                        Item_Type : Apply_On) return Boolean is
   --  begin
   --     for F of reverse Filter_Chain loop
   --        -- The item may be included and excluded several time :
   --        -- looking into the chain in reverse order is an optimized way to
   --        -- have the bahavior "the last to spoke has right".
   --        if (F.Target = Item_Type or F.Target = Apply_To_All)
   --          and then Contains (S, F.Pattern)
   --          and then F.Mode = Exclude
   --        then
   --           Put_Debug_Line ("Is_Filtered (" & S &
   --                             ", " & Item_Type'Image &
   --                             ", Mode = " & F.Mode'Image & ") return True");
   --           return True;
   --           -- exit;
   --        end if;
   --     end loop;
   --     Put_Debug_Line ("Is_Filtered (" & S &
   --                       ", " & Item_Type'Image & ") return False");
   --     return False;
   --  end Is_Filtered;
   --
   --  -- --------------------------------------------------------------------------
   --  function Is_Selected (S : String;
   --                        Item_Type : Apply_On) return Boolean is
   --  begin
   --     -- Fixme : duplicated code Is_Filtered Is_Selected
   --     for F of reverse Filter_Chain loop
   --        -- The item may be included and excluded several time :
   --        -- looking into the chain in reverse order is an optimized way to
   --        -- have the bahavior "the last to spoke has right".
   --        if (F.Target = Item_Type or F.Target = Apply_To_All)
   --          and then Contains (S, F.Pattern)
   --          and then F.Mode = Include
   --        then
   --           Put_Debug_Line ("Is_Selected (" & S &
   --                             ", " & Item_Type'Image &
   --                             ", Mode = " & F.Mode'Image & ") return True");
   --           return True;
   --           -- exit;
   --        end if;
   --     end loop;
   --     Put_Debug_Line ("Is_Selected (" & S &
   --                       ", " & Item_Type'Image & ") return False");
   --     return False;
   --  end Is_Selected;

   -- -------------------------------------------------------------------------
   function Short_Image (F : Filter) return String is
      (F.Mode'Image & " """ & (+F.Pattern) & """ on " & F.Target'Image);


end BBT.Tests.Filter_List;
