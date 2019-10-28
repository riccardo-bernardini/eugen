pragma Ada_2012;
package body Finite_State_Scanners is

   ----------------
   -- Add_Branch --
   ----------------

   procedure Add_Branch
         (Table : in out Automata_Table;
          From  : State_Type;
          On    : Ada.Strings.Maps.Character_Set;
          To    : State_Type)
   is
      use Ada.Strings.Maps;

      Set : constant Character_Sequence := To_Sequence (On);
   begin
      for C of Set loop
         Table.Transitions (From, C) := (Class       => To_State,
                                         Destination => To);
      end loop;
   end Add_Branch;

   -----------------
   -- Add_Default --
   -----------------

   procedure Add_Default
         (Table : in out Automata_Table;
          From  : State_Type;
          To    : State_Type)
   is
   begin
      for C in Character loop
         if Table.Transitions (From, C).Class = Empty then
            Table.Transitions (From, C) :=  (Class       => To_State,
                                             Destination => To);
         end if;
      end loop;
   end Add_Default;

   ---------------
   -- Add_Final --
   ---------------

   procedure Add_Final
         (Table  : in out Automata_Table;
          From   : State_Type;
          On     : Ada.Strings.Maps.Character_Set;
          Result : Token_Type)
   is
   begin
      for C of Ada.Strings.Maps.To_Sequence (On) loop
         Table.Transitions (From, C) := (Class  => To_Final,
                                         Result => Result);
      end loop;
   end Add_Final;

   ---------------
   -- Add_Final --
   ---------------

   procedure Add_Shortcut
         (Table  : in out Automata_Table;
          From   : State_Type;
          On     : String;
          Result : Token_Type)
   is
   begin
      Table.Shortcuts (From).Append ((Length   => On'Length,
                                      Shortcut => On,
                                      Result   => Result));
   end Add_Shortcut;

   -----------------
   -- Add_Default --
   -----------------

   procedure Add_Default
         (Table  : in out Automata_Table;
          From   : State_Type;
          Result : Token_Type)
   is
   begin
      for C in Character loop
         if Table.Transitions (From, C).Class = Empty then
            Table.Transitions (From, C) :=  (Class       => To_Final,
                                             Result      => Result);
         end if;
      end loop;
   end Add_Default;

   ----------
   -- Scan --
   ----------

   function Scan
         (Input  : String;
          Automa : Automata_Table;
          Skip   : String := " ")
          return Token_List
   is
      use Ada.Strings.Maps;

      Cursor : Positive := Input'First;

      Result        : Token_List;
      Current_State : State_Type;
      Image_First   : Positive;

      Spaces        : constant Character_Set := To_Set (Skip);

      function End_Of_Input return Boolean
      is (Cursor > Input'Last);

      function Current_Char return Character
      is (if End_Of_Input then
             Character'Val (0)
          else
             Input (Cursor));

      procedure Next_Char is
      begin
         if not End_Of_Input then
            Cursor := Cursor + 1;
         end if;
      end Next_Char;

      procedure Skip_Spaces is
      begin
         while Is_In (Current_Char, Spaces) loop
            Next_Char;
         end loop;
      end Skip_Spaces;

      procedure Start_New_Token is
      begin
         Current_State := Start_State;
         Skip_Spaces;
         Image_First := Cursor;
      end Start_New_Token;

      procedure Append_Token (To    : in out Token_List;
                              Token : Token_Type)
      is
         Image : constant String := Input (Image_First .. Cursor - 1);
      begin
         To.L.Append (Token_Descriptor'(Length => Image'Length,
                                        Token  => Token,
                                        Image  => Image));
      end Append_Token;
   begin
      Start_New_Token;

      while not End_Of_Input loop
         case Automa.Transitions (Current_State, Current_Char).Class is
            when Empty =>
               for Shortcut of Automa.Shortcuts (Current_State) loop
                  raise Program_Error with "Shortcuts not implemented yet";
                  pragma Compile_Time_Warning (True, "Shortcuts not implemented");
               end loop;
               raise Constraint_Error;

            when To_State =>
               Current_State := Automa.Transitions (Current_State, Current_Char).Destination;
               Next_Char;

            when To_Final =>
               Append_Token (Result, Automa.Transitions (Current_State, Current_Char).Result);
               Next_Char;
               Start_New_Token;
         end case;

      end loop;

      return Result;
   end Scan;

end Finite_State_Scanners;
