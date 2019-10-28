pragma Ada_2012;

with GNAT.Regpat;   use GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;

package body Regexp_Readers.Generic_Readers is
   Matcher : constant Pattern_Matcher := Compile (To_String (Regexp.Regexp));
   N_Paren : constant Natural := Paren_Count (Matcher);
   Sub_Expression_Index : constant Natural := Regexp.Sub_Expression_Index;

   procedure Reader
     (Input    : in     String;
      Success  :    out Boolean;
      Consumed :    out Natural;
      Result   :    out Result_Type)
   is
      Matches : Match_Array (0 .. Sub_Expression_Index);
   begin
--        Put_Line (To_String (Regexp.Regexp) & Sub_Expression_Index'Image);
      if N_Paren < Sub_Expression_Index then
         raise Constraint_Error;
      end if;

      Match (Self       => Matcher,
             Data       => Input,
             Matches    => Matches);

      if Matches (Sub_Expression_Index) = No_Match then
         Success := False;
         Consumed := 0;
      else
         declare
            M : constant Match_Location := Matches (Sub_Expression_Index);
         begin
--              Put_Line ("(" & Input & ")" & M.First'Image & M.Last'Image & "**"
--                        & "(" & Input (M.First .. M.Last) & ")");

            Success := True;
            Result := Convert (Input (M.First .. M.Last));
            Consumed := Matches (0).Last - Input'First + 1;
         end;
      end if;
   end Reader;
   end Regexp_Readers.Generic_Readers;
