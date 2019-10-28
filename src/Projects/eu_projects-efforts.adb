pragma Ada_2012;
with Tokenize.Token_Vectors;
with Ada.Strings.Fixed;

package body EU_Projects.Efforts is

   -----------
   -- Parse --
   -----------

   function Parse (Spec : String) return Effort_Maps.Map is
      use Tokenize;
      use Tokenize.Token_Vectors;

      use EU_Projects.Nodes.Partners;
      use Ada.Strings.Fixed;
      use Ada.Strings;

      Result : Effort_Maps.Map := Effort_Maps.Empty_Map;

   begin
      if Spec = "" then
         return Effort_Maps.Empty_Map;
      end if;

      for Partner_Spec of To_Vector (Split (Spec, ',', False)) loop
         declare
            use type Ada.Containers.Count_Type;

            Parts : constant Token_Vectors.Vector :=
                      Token_Vectors.To_Vector (Split (Partner_Spec, "=:", False));
            Name  : Partner_Label;
         begin
            if Parts.Length /= 2 then
               raise Bad_Effort_List with "Bad partner effort spec";
            end if;

            Name := Partner_Label'(To_ID (Trim (Parts.First_Element, Both)));

            if Result.Contains (Name) then
               raise Bad_Effort_List with "Duplicated partner in effort list";
            end if;

            Result.Include (Key      => Name,
                            New_Item => Person_Months (Natural'Value (Parts.Last_Element)));

         end;
      end loop;

      return Result;
   end Parse;

end EU_Projects.Efforts;
