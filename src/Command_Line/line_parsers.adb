----------------------------------------------------------------------------
--            Generic Command Line Parser (gclp)
--
--               Copyright (C) 2012, Riccardo Bernardini
--
--      This file is part of gclp.
--
--      gclp is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 2 of the License, or
--      (at your option) any later version.
--
--      gclp is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with gclp.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------
--
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Characters.Handling;

use Ada;
use Ada.Strings;
use Ada.Strings.Fixed;
with Ada.Directories;

package body Line_Parsers is

   function To_S (X : Unbounded_String) return String
                  renames To_String;

   function To_U (X : String) return Unbounded_String
                  renames To_Unbounded_String;

   package Name_Lists is
         new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   function Parse_Name (Name           : String;
                        Case_Sensitive : Boolean) return Name_Lists.List;

   procedure Add_Parameter
         (Parser     : in out Line_Parser;
          Name       : String;
          If_Missing : Missing_Action := Ignore;
          Default    : String;
          Handler    : Handler_Access)
   is
      Names : constant Name_Lists.List := Parse_Name (Name, Parser.Case_Sensitive);
   begin
      Parser.Parameters.Append
            (Parameter_Descriptor'(If_Missing    => If_Missing,
                                   Name          => To_U (Name),
                                   Handler       => Handler,
                                   Standard_Name => To_U (Names.First_Element),
                                   Default       => To_U (Default)));

      for Single_Name of Names loop
         if Parser.Name_Table.Contains (Single_Name) then
            raise Constraint_Error;
         end if;

         Parser.Name_Table.Insert (Single_Name, Parser.Parameters.Last_Index);
      end loop;
   end Add_Parameter;




   --     type Parameter_Descriptor_Array is
   --           array (Parameter_Index range <>) of Parameter_Descriptor;


   --------------------
   -- Case_Normalize --
   --------------------

   -- If the user required case insensitive matching, force the
   -- name to lower case
   procedure Case_Normalize (Name           : in out String;
                             Case_Sensitive : Boolean) is
   begin
      if not Case_Sensitive then
         Translate (Name, Maps.Constants.Lower_Case_Map);
      end if;
   end Case_Normalize;

   function Parse_Name (Name           : String;
                        Case_Sensitive : Boolean) return Name_Lists.List
   is
      ------------------
      -- Trimmed_Name --
      ------------------

      function Trimmed_Name (Name  : String)
                             return String
      is
         Trimmed :  String := Fixed.Trim (Name, Both);
      begin
         if Trimmed = "" then
            raise Constraint_Error
                  with "Empty alternative in label '" & Name & "'";
         else
            Case_Normalize (Trimmed, Case_Sensitive);
            return Trimmed;
         end if;
      end Trimmed_Name;

      Result    : Name_Lists.List;
      First     : Natural;
      Comma_Pos : Natural;
   begin
      if Fixed.Index (Name, "=") /= 0 then
         raise Constraint_Error with "Option label '" & Name & "' has '='";
      end if;

      if Name (Name'Last) = ',' then
         raise Constraint_Error
               with "Option label '" & Name & "' ends with ','";
      end if;

      First := Name'First;
      loop
         pragma Assert (First <= Name'Last);

         Comma_Pos := Fixed.Index (Name (First .. Name'Last), ",");
         exit when Comma_Pos = 0;

         if First = Comma_Pos then
            -- First should always point to the beginning of a
            -- label, therefore it cannot be Buffer(First) = ','
            raise Constraint_Error
                  with "Wrong syntax in Option label '" & Name & "'";
         end if;

         pragma Assert (Comma_Pos > First);

         Result.Append (Trimmed_Name (Name (First .. Comma_Pos - 1)));

         First := Comma_Pos + 1;

         -- It cannot be First > Buffer'Last since Buffer(Comma_Pos) = '='
         -- and Buffer(Buffer'Last) /= ','
         pragma Assert (First <= Name'Last);
      end loop;

      pragma Assert (First <= Name'Last);

      Result.Append (Trimmed_Name (Name (First .. Name'Last)));

      return Result;
   end Parse_Name;


   ------------
   -- Create --
   ------------

   function Create
         (Case_Sensitive : Boolean := True;
          Normalize_Name : Boolean := True;
          Help_Line      : String := "")
          return Line_Parser
   is
   begin
      return Line_Parser'(Case_Sensitive => Case_Sensitive,
                          Normalize_Name => Normalize_Name,
                          Help_Line      => To_U (Help_Line),
                          Parameters     => Parameter_Vectors.Empty_Vector,
                          Name_Table     => Name_To_Index_Maps.Empty_Map);
   end Create;

   -----------
   -- Slurp --
   -----------

   function Slurp (Filename       : String;
                   Skip_Comments  : Boolean := True;
                   Comment_Char   : Character := '#';
                   Comment_Strict : Boolean := False)
                    return String_Vectors.Vector
   is
      use Ada.Text_IO;
      use Ada.Directories;
      use Ada.Characters.Handling;

      function Is_Empty (X : String) return Boolean
      is (for all Ch of X => Is_Space(Ch));

      ----------------
      -- Is_Comment --
      ----------------

      function Is_Comment (X : String) return Boolean
      is
         Idx : constant Natural := Index (X, Comment_Char & "");
      begin
         if Idx = 0 then
            return False;
         end if;

         if Comment_Strict then
            return Idx = X'First;
         else
            return (for all N in X'First .. Idx - 1 => Is_Space (X (N)));
         end if;
      end Is_Comment;

      Input : File_Type;
      Result : String_Vectors.Vector;
   begin
      if not Exists (Filename) or else Kind (Filename) /= Ordinary_File then
         return String_Vectors.Empty_Vector;
      end if;

      Open (File => input,
            Mode => In_File,
            Name => Filename);

      while not End_Of_File (Input) loop
         declare
            Line : constant String := Get_Line (Input);
         begin
            if not Is_Empty (Line) then
               if not Skip_Comments or not Is_Comment (Line) then
                  Result.Append (Line);
               end if;
            end if;
         end;
      end loop;

      Close (Input);

      return Result;
   end Slurp;


   ------------------------
   -- Parse_Command_Line --
   ------------------------


   procedure Parse_Command_Line
     (Parser         : Line_Parser;
      Extend_By      : String_Vectors.Vector := String_Vectors.Empty_Vector;
      Help_Output    : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error) is

      package String_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);


      ---------------------
      -- Split_Parameter --
      ---------------------

      procedure Split_Parameter (Param : in     String;
                                 Name  :    out Unbounded_String;
                                 Value :    out Unbounded_String)
      is
         Idx : Natural;
      begin
         Idx := Index (Source  => Param,
                       Pattern => "=");

         if (Idx = 0) then
            Name  := To_U (Param);
            Value := Null_Unbounded_String;
         else
            Name  := To_U (Param (Param'First .. Idx - 1));
            Value := To_U (Param (Idx + 1 .. Param'Last));
         end if;
      end Split_Parameter;

      function Missing_Message (Missing  : String_Lists.List)
                                return String
      is
         function Join (Item : String_Lists.List) return String is
            Result : Unbounded_String;

            procedure Append (Pos : String_Lists.Cursor) is
            begin
               if Result /= Null_Unbounded_String then
                  Result := Result & ", ";
               end if;

               Result := Result & "'" & String_Lists.Element (Pos) & "'";
            end Append;
         begin
            Item.Iterate (Append'Access);

            return To_String (Result);
         end Join;

         use type Ada.Containers.Count_Type;
      begin
         if Missing.Length = 1 then
            return "Missing mandatory option " & Join (Missing);
         else
            return "Missing mandatory options: " & Join (Missing);
         end if;
      end Missing_Message;

      function Collect_Parameters (Extra : String_Vectors.Vector)
                                   return String_Vectors.Vector
      is
         Result : String_Vectors.Vector;
      begin
         for Idx in 1 .. Command_Line.Argument_Count loop
            Result.Append (Command_Line.Argument (Idx));
         end loop;

         Result.Append (Extra);

         return Result;
      end Collect_Parameters;


      Name          : Unbounded_String;
      Value         : Unbounded_String;

      use Name_To_Index_Maps;

      Position   : Name_To_Index_Maps.Cursor;
      Param_Idx  : Parameter_Index;

      Arguments : constant String_Vectors.Vector := Collect_Parameters (Extend_By);
   begin
      for Pos  in Arguments.First_Index .. Arguments.Last_Index loop
         Split_Parameter (Arguments (Pos), Name, Value);
         declare
            N              : String := To_S (Name);
            V              : constant String := To_S (Value);
            Handler        : Handler_Access;
            This_Parameter : Parameter_Descriptor;
         begin
            Case_Normalize (N, Parser.Case_Sensitive);

            Position := Parser.Name_Table.Find (N);

            if Position = No_Element then
               raise Bad_Command with "Option '" & To_S (Name) & "' unknown";
            end if;

            Param_Idx := Name_To_Index_Maps.Element (Position);
            This_Parameter := Parser.Parameters (Param_Idx);
            Handler := This_Parameter.Handler;

            if Handler.Is_Set and not Handler.Reusable then
               raise Bad_Command with "Option '" & N & "' given twice";
            end if;

            Handler.Receive (Name     => (
                                          if Parser.Normalize_Name then
                                             To_S (This_Parameter.Standard_Name)
                                          else
                                             N
                                         ),
                             Value    => V,
                             Position => Pos);
         end;
      end loop;

      declare
         Missing   : String_Lists.List;
      begin
         for Parameter of Parser.Parameters loop
            if not Parameter.Handler.Is_Set then
               case Parameter.If_Missing is

               when Die =>
                  Missing.Append (To_S (Parameter.Standard_Name));

                  when Use_Default =>
                     Parameter.Handler.Receive (Name     => To_S (Parameter.Standard_Name),
                                                Value    => To_S (Parameter.Default),
                                                Position => No_Position);

                  when Ignore =>
                     null;
               end case;
            end if;
         end loop;

         if not Missing.Is_Empty then
            raise Bad_Command with Missing_Message (Missing);
         end if;
      end;
   exception
      when Bad_Command =>
         if Parser.Help_Line /= Null_Unbounded_String then
            Ada.Text_IO.Put_Line (File => Help_Output,
                                  Item => To_S (Parser.Help_Line));
         end if;

         raise;
   end Parse_Command_Line;

end Line_Parsers;
--
--     ---------------------
--     -- Normalized_Form --
--     ---------------------
--
--     function Normalized_Form (Parser : Line_Parser;
--                               X      : String) return String
--     is
--        Names  : constant Name_Lists.List := Parse_Name (X, Parser.Case_Sensitive);
--        Result :  String := Names.First_Element;
--     begin
--        Case_Normalize (Result, Parser.Case_Sensitive);
--        return Result;
--     end Normalized_Form;
--
--     ---------------------
--     -- Fill_Name_Table --
--     ---------------------
--     procedure Fill_Name_Table (Parameters     : in     Parameter_Descriptor_Array;
--                                Name_Table     : in out Name_To_Index_Maps.Map;
--                                Standard_Names :    out Name_Array)
--           with
--                 Pre =>
--                       Parameters'First = Standard_Names'First
--                       and
--                             Parameters'Last = Standard_Names'Last;
--
--     -- Fill the Parameter Name -> parameter index table with the
--     -- parameter names
--     procedure Fill_Name_Table (Parser         : Line_Parser;
--                                Parameters     : in     Parameter_Descriptor_Array;
--                                Name_Table     : in out Name_To_Index_Maps.Map;
--                                Standard_Names :    out Name_Array)
--     is
--
--
--        use Name_Lists;
--
--        ----------------
--        -- Parse_Name --
--        ----------------
--
--
--        Option_Names : Name_Lists.List;
--        Position : Name_Lists.Cursor;
--
--        Name : Unbounded_String;
--     begin
--        for Idx in Parameters'Range loop
--           Option_Names := Parse_Name (Parameters (Idx).Name);
--
--           Position := Option_Names.First;
--           Standard_Names (Idx) :=  Name_Lists.Element (Position);
--
--           while Position /= No_Element loop
--              Name := Name_Lists.Element (Position);
--              Name_Lists.Next (Position);
--
--              Case_Normalize (Parser, Name);
--
--              if Name_Table.Contains (Name) then
--                 raise Constraint_Error
--                       with "Ambiguous label '" & To_S (Name) & "'";
--              end if;
--
--              Name_Table.Insert (Name, Idx);
--           end loop;
--        end loop;
--     end Fill_Name_Table;


--     ----------------
--     -- To_Natural --
--     ----------------
--
--     function To_Natural (X : Unbounded_String)
--                          return Natural is
--     begin
--        if X = Null_Unbounded_String then
--           raise Bad_Command with "Invalid integer '" & To_S (X) & "'";
--        end if;
--
--        return Natural'Value (To_S (X));
--     end To_Natural;
--
--     --------------
--     -- To_Float --
--     --------------
--
--     function To_Float (X : Unbounded_String)
--                        return Float is
--     begin
--        if X = Null_Unbounded_String then
--           raise Bad_Command with "Invalid Float '" & To_S (X) & "'";
--        end if;
--
--        return Float'Value (To_S (X));
--     end To_Float;

