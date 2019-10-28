pragma Ada_2012;
with Tokenize.Token_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Symbolic_Expressions.Parsing is
   Verbose : constant Boolean := False;
   -- Set to true to enable debug prints

   -------------
   -- Exactly --
   -------------

   function Exactly  (N : Natural) return Parameter_Count
   is
   begin
      return Parameter_Count'(Min => N, Max => N);
   end Exactly;

   --------------
   -- At_Least --
   --------------

   function At_Least (N : Natural) return Parameter_Count
   is
   begin
      return Parameter_Count'(Min => N, Max => Natural'Last);
   end At_Least;

   -------------
   -- Between --
   -------------

   function Between (Min, Max : Natural) return Parameter_Count
   is
   begin
      return Parameter_Count'(Min => Min, Max => Max);
   end Between;

   ---------------------
   -- Define_Variable --
   ---------------------

   procedure Define_Variable (Container  : in out ID_Table_Type;
                              Name       : Variable_Name)
   is
   begin
      Container.T.Include (Identifier (Name), (Class => Var));
   end Define_Variable;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function (Container  : in out ID_Table_Type;
                              Name       : Function_Name;
                              N_Params   : Parameter_Count)
   is
   begin
      Container.T.Include (Identifier (Name), (Class => Funct, N_Param => N_Params));
   end Define_Function;

   -------------------
   -- Is_Acceptable --
   -------------------

   function Is_Acceptable (N_Param : Natural;
                           Limits  : Parameter_Count)
                           return Boolean
   is
   begin
      return N_Param >= Limits.Min and N_Param <= Limits.Max;
   end Is_Acceptable;

   function "&" (X : String ; ID : Identifier) return String
   is (X & " '" & ID_Image (ID) & "' ");

   function "&" (ID : Identifier; X : String) return String
   is (" '" & ID_Image (ID) & "' " & X);

   --     function Msg (ID : Identifier; Post : String) return String
   --     is ("'" & ID_Image (ID) & "' " & Post);
   --
   --     function Msg (Pre : String; ID : Identifier; Post : String := "") return String
   --     is (Pre & " '" & ID_Image (ID) & "' " & Post);


   -----------
   -- Parse --
   -----------

   function Parse (Input             : String;
                   ID_Table          : ID_Table_Type := Empty_ID_Table;
                   On_Unknown_ID     : Unknown_ID_Action_Type := OK;
                   Prefixes          : String := "";
                   Prefix_Separator  : String := ".";
                   On_Multiple_Match : Multiple_Match_Action := Allow_Unprefixed)
                   return Symbolic_Expression
   is
      pragma Unreferenced (Prefix_Separator);
      use Tokenize;

      Buffer       : constant String := Input;
      Cursor       : Positive;
      Current_Char : Character;
      EOS          : constant Character := Character'Val (0);

      type ID_Array is array (Positive range <>) of Identifier;

      function To_ID_Array (X : Token_Vectors.Vector) return ID_Array
      is
         Result : ID_Array (X.First_Index .. X.Last_Index);
      begin
         for I in Result'Range loop
            declare
               S        : constant String := X (I);
               OK       : Boolean;
               Consumed : Natural;
            begin
               Read_Identifier (Input    => S,
                                Success  => OK,
                                Consumed => Consumed,
                                Result   => Result (I));

               if not OK or Consumed < S'Length then
                  raise Parsing_Error;
               end if;
            end;
         end loop;

         return Result;
      end To_ID_Array;

      Prefix_List  : constant ID_Array :=
                       To_ID_Array (Token_Vectors.To_Vector (Split (Prefixes)));

      procedure Init_Scanner is
      begin
         Cursor := Buffer'First;

         while Cursor <= Buffer'Last and then Buffer (Cursor) = ' ' loop
            Cursor := Cursor + 1;
         end loop;

         if Cursor <= Buffer'Last then
            Current_Char := Buffer (Cursor);
         else
            Current_Char := EOS;
         end if;
      end Init_Scanner;

      procedure Next_Char (Skip_Spaces : Boolean := True) is
      begin
         loop
            if Cursor >= Buffer'Last then
               Current_Char := EOS;
            else
               Cursor := Cursor + 1;
               Current_Char := Buffer (Cursor);
            end if;

            exit when (not Skip_Spaces) or Current_Char /= ' ';
         end loop;
      end Next_Char;

      procedure Expect (What : Character) is
      begin
         if Current_Char /= What then
            raise Parsing_Error
                  with "Expecting '" & What & "' got '" & Current_Char & "'";
         else
            Next_Char;
         end if;
      end Expect;

      function Remaining return String is
      begin
         return Buffer (Cursor .. Buffer'Last);
      end Remaining;

      Level : Natural := 0;

      function Indent return String is
         use Ada.Strings.Fixed;
      begin
         return (Level * 3) * " ";
      end Indent;

      procedure Down_Level is
      begin
         Level := Level + 1;
      end Down_Level;

      procedure Up_Level is
      begin
         Level := Level - 1;
      end Up_Level;

      procedure Ecco (X : String) is
      begin
         if Verbose then
            Ada.Text_Io.Put_Line (Indent
                                  & "Calling "
                                  & X & "[" & Remaining & "]"
                                  & "'" & Current_Char & "'");
            Down_Level;
         end if;
      end Ecco;

      procedure Fine is
      begin
         if Verbose then
            Up_Level;
            Ada.Text_Io.Put_Line (Indent & "done " & "[" & Remaining & "]"
                                  & "'" & Current_Char & "'");
         end if;
      end Fine;

      procedure Advance (N : Positive) is
      begin
         Cursor := Cursor + N - 1;
         Next_Char;
      end Advance;

      function Parse_Expr return Node_Access;

      --        function Parse_Identifier return Bounded_ID is
      --           use Ada.Strings.Maps;
      --           use Bounded_IDs;
      --
      --           Result   : Bounded_ID;
      --           ID_Chars : constant Character_Set :=
      --                        To_Set (Character_Range'('a', 'z')) or
      --              To_Set (Character_Range'('A', 'Z')) or
      --              To_Set (Character_Range'('0', '9')) or
      --              To_Set ('.') or
      --              To_Set ('_');
      --        begin
      --           Ecco ("ID");
      --
      --           while Is_In (Current_Char, ID_Chars) loop
      --              Result := Result & Current_Char;
      --              Next_Char (Skip_Spaces => False);
      --           end loop;
      --
      --           if Current_Char = ' ' then
      --              Next_Char;
      --           end if;
      --
      --           Fine;
      --           return Result;
      --        end Parse_Identifier;

      procedure Parse_Parameter_List (Parameters : out Parameter_Array;
                                      N_Params   : out Natural)
      is
      begin
         Ecco ("par-list");

         if Current_Char = ')' then
            N_Params := 0;

         else
            N_Params := 1;
            Parameters (1) := Parse_Expr;

            while Current_Char = ',' loop
               Next_Char;
               N_Params := N_Params + 1;
               Parameters (N_Params) := Parse_Expr;
            end loop;
         end if;

         Expect (')');
         Fine;
      exception
         when others =>
            for I in Parameters'First .. N_Params - 1 loop
               Free (Parameters (I));
            end loop;

            raise;
      end Parse_Parameter_List;

      procedure Resolve_Identifier (Raw_ID           : in     Identifier;
                                    Pos              :    out ID_Tables.Cursor;
                                    N_Matches        :    out Natural;
                                    Resolved         :    out Identifier;
                                    No_Prefix_Needed : out Boolean)
            with Post =>
                  (
                         (ID_Tables."=" (Pos, ID_Tables.No_Element) = (N_Matches = 0))
                  );


      procedure Resolve_Identifier (Raw_ID           : in     Identifier;
                                    Pos              :    out ID_Tables.Cursor;
                                    N_Matches        :    out Natural;
                                    Resolved         :    out Identifier;
                                    No_Prefix_Needed :    out Boolean)
      is
         use ID_Tables;
      begin
         No_Prefix_Needed := False;
         N_Matches := 0;

         Pos := ID_Table.T.Find (Raw_ID);

         if Pos /= No_Element then
            Resolved := Raw_ID;
            No_Prefix_Needed := True;
            N_Matches := 1;
         end if;


         declare
            Tmp : Identifier;
         begin
            for I in Prefix_List'Range loop
               Tmp := Join (Prefix_List (I), Raw_ID);

               Pos := ID_Table.T.Find (Tmp);

               if Pos /= No_Element then
                  N_Matches := N_Matches + 1;

                  if N_Matches = 1 then
                     Resolved := Tmp;
                  end if;
               end if;
            end loop;
         end;

         pragma Assert
               ((Pos /= No_Element) = (N_Matches > 0));
      end Resolve_Identifier;


      function Parse_Simple return Node_Access is
         use ID_Tables;


         function Try_Parenthesis (Result : out Node_Access) return Boolean
         is
         begin
            if Current_Char = '(' then
               Next_Char;
               Result := Parse_Expr;
               Expect (')');
               return True;
            else
               return False;
            end if;
         end Try_Parenthesis;

         function Try_Scalar (Result : out Node_Access)
                              return Boolean
         is
            Success  : Boolean;
            Consumed : Natural;
            Val      : Scalar_Type;
         begin
            Read_Scalar (Remaining, Success, Consumed, Val);

            if Success then
               Advance (Consumed);

               Result := new Node_Type'(Class => Const,
                                        Value => Val);
               return True;
            else
               return False;
            end if;
         end Try_Scalar;

         --------------------
         -- Try_Identifier --
         --------------------

         function Try_Identifier (ID : out Identifier)
                                  return Boolean
         is
            Success  : Boolean;
            Consumed : Natural;
         begin
--              Put_Line ("TRY ID");
            Read_Identifier (Remaining, Success, Consumed, ID);

            if Success then
--                 Put_Line ("SUCCESS (" & ID_Image(Id) & ")");
               Advance (Consumed);

               return True;
            else

--                 Put_Line ("NO");
               return False;
            end if;
         end Try_Identifier;

         ---------------------
         -- Handle_Function --
         ---------------------

         function Handle_Function (ID  : Identifier;
                                   Pos : ID_Tables.Cursor)
                                   return Node_Access
         is
            Parameters     : Parameter_Array;
            N_Params       : Natural;
         begin
            if Pos = No_Element then
               if On_Unknown_ID /= OK then
                  raise Parsing_Error  with "Unknown ID '" & ID;
               end if;
            else
               if Element (Pos).Class /= Funct then
                  raise Parsing_Error with "Variable" & ID & "used as function";
               end if;
            end if;

            Next_Char;

            Parse_Parameter_List (Parameters, N_Params);

            if Pos /= No_Element then
               pragma Assert (Element (Pos).Class = Funct);

               if not Is_Acceptable (N_Params, Element (Pos).N_Param) then
                  raise Parsing_Error
                        with "Wrong # of arguments";
               end if;
            end if;


            return new Node_Type'(Class      => Fun_Call,
                                  Fun_Name   => Function_Name (ID),
                                  Parameters => Parameters,
                                  N_Params   => N_Params);

         end Handle_Function;

         ---------------------
         -- Handle_Variable --
         ---------------------

         function Handle_Variable (ID  : Identifier;
                                   Pos : ID_Tables.Cursor)
                                   return Node_Access
         is
         begin
            if Pos /= No_Element and then Element (Pos).Class /= Var then
               raise Parsing_Error with "Function '" & ID & "' used as variable";
            end if;

            return new Node_Type'(Class    => Var,
                                  Var_Name => Variable_Name (ID));

         end Handle_Variable;


         Result         : Node_Access := null;
         Raw_ID         : Identifier;
         ID             : Identifier;
         Pos            : ID_Tables.Cursor;
         N_Matches      : Natural;
         No_Prefix      : Boolean;
      begin
--           Put_Line ("PARSING '" & Input & "'");

         Ecco ("simple");

         if Try_Parenthesis (Result) then
            return Result;

         elsif Try_Scalar (Result) then
            return Result;

         elsif Try_Identifier (Raw_ID) then
            Resolve_Identifier (Raw_ID           => Raw_ID,
                                Pos              => Pos,
                                N_Matches        => N_Matches,
                                Resolved         => ID,
                                No_Prefix_Needed => No_Prefix);

            if Pos = No_Element then
               if  On_Unknown_ID = Die then
                  raise Parsing_Error with "Unknown ID" & Raw_ID;
               end if;
            end if;

            if N_Matches > 1 then
               case On_Multiple_Match is
                  when Die =>
                     raise Parsing_Error with Raw_ID & "has multiple matches";

                  when Allow_Unprefixed =>
                     if not No_Prefix then
                        raise Parsing_Error with Raw_ID & "has multiple prefixed matches";
                     end if;

                  when Allow =>
                     null;
               end case;
            end if;

            pragma Assert (if On_Multiple_Match = Die then N_Matches <= 1);


            if Current_Char = '(' then
               return Handle_Function (ID, Pos);

            else
               return Handle_Variable (ID, Pos);
            end if;

         else
            raise Parsing_Error;

         end if;

      exception
         when others =>
            Free (Result);
            raise;
      end Parse_Simple;

      function Parse_Fact return Node_Access is
         Result   : Node_Access := null;
         Operator : Character;
         Operand  : Node_Access;
      begin
         Ecco ("fact");

         if Current_Char = '+' or Current_Char = '-' then
            Operator := Current_Char;
            Next_Char;

            Operand := Parse_Simple;

            case Operator is
               when '+' =>
                  Result := new Node_Type'(Class => Unary_Plus,
                                           Term  => Operand);

               when '-' =>
                  Result := new Node_Type'(Class => Unary_Minus,
                                           Term  => Operand);

               when others =>
                  -- We should never arrive here
                  raise Program_Error;
            end case;
         else
            Result := Parse_Simple;
         end if;

         Fine;
         return Result;
      exception
         when others =>
            Free (Result);
            raise;
      end Parse_Fact;

      function Parse_Term return Node_Access is
         Result   : Node_Access := null;
         Operator : Character;
         Operand  : Node_Access;
      begin
         Ecco ("term");

         Result := Parse_Fact;

         while Current_Char = '*' or Current_Char = '/' loop
            Operator := Current_Char;
            Next_Char;

            Operand := Parse_Fact;
            case Operator is
               when '*' =>
                  Result := new Node_Type'(Class => Mult,
                                           Left  => Result,
                                           Right => Operand);

               when '/' =>
                  Result := new Node_Type'(Class => Div,
                                           Left  => Result,
                                           Right => Operand);

               when others =>
                  -- We should never arrive here
                  raise Program_Error;
            end case;
         end loop;

         Fine;
         return Result;
      exception
         when others =>
            Free (Result);
            raise;
      end Parse_Term;

      function Parse_Expr return Node_Access is
         Result   : Node_Access := null;
         Operator : Character;
         Operand  : Node_Access;
      begin
         Ecco ("expr");

         Result := Parse_Term;

         while Current_Char = '+' or Current_Char = '-' loop
            Operator := Current_Char;
            Next_Char;

            Operand := Parse_Term;
            case Operator is
               when '+' =>
                  Result := new Node_Type'(Class => Sum,
                                           Left  => Result,
                                           Right => Operand);

               when '-' =>
                  Result := new Node_Type'(Class => Sub,
                                           Left  => Result,
                                           Right => Operand);

               when others =>
                  -- We should never arrive here
                  raise Program_Error;
            end case;
         end loop;

         Fine;
         return Result;
      exception
         when others =>
            Free (Result);
            raise;
      end Parse_Expr;

      Result : Symbolic_Expression;
   begin
      Init_Scanner;

      Result.Expr := Parse_Expr;

      return Result;
   end Parse;


end Symbolic_Expressions.Parsing;
