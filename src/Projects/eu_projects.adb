pragma Ada_2012;
with Tokenize.Token_Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package body EU_Projects is
   function Verbose_To_ID (X : String) return Dotted_Identifier
   is
   begin
      Put_Line ("TT(" & X & ")" & Is_Valid_ID (X)'Image);
      return To_Bounded_String (X);
   end Verbose_To_ID;

   ----------------
   -- To_ID_List --
   ----------------

   function To_ID_List (Input      : String;
                        Separators : String := " ,")
                         return ID_List
   is
      use Tokenize;

      Result : ID_List;
      Splitted : constant Token_Vectors.Vector :=
                   Token_Vectors.To_Vector(Split (To_Be_Splitted    => Input,
                                                  Separator         => Separators,
                                                  Collate_Separator => True));

      OK       : Boolean;
      Consumed : Natural;
      Tmp      : Dotted_Identifier;

   begin
      for ID of Splitted loop
--           Put_Line ("[" & Id & "]");
         ID_Readers.Reader (Input    => ID,
                            Success  => OK,
                            Consumed => Consumed,
                            Result   => Tmp);

--           Put_Line (Ok'Image & " " & Consumed'Image & "(" & To_String (Tmp) & ")");

         if not OK or Consumed < ID'Length then
            raise Bad_Identifier with "Bad ID in '" & ID & "'";
         else
            Result.Append (Tmp);
         end if;
      end loop;

      return Result;
   end To_ID_List;

end EU_Projects;
