pragma Ada_2012;

with Ada.Characters.Handling;
with Ada.Strings.Bounded;

package body Config_String_Parsers is

   --------------------------
   -- Add_Parameter_Syntax --
   --------------------------

   procedure Add_Parameter_Syntax
     (Syntax         : in out Syntax_Descriptor;
      Parameter_Name : Name_Type;
      If_Missing     : Missing_Action;
      Default        : Value_Type := No_Value)
   is
   begin
      Syntax.S.Insert
        (Key      => Parameter_Name,
         New_Item => (If_Missing => If_Missing,
                      Default    => Value_Holders.To_Holder (Default)));
   end Add_Parameter_Syntax;

   ------------
   -- Create --
   ------------

   function Config
     (Expect_Header       : Header_Expected := No;
      Name_Start          : Character_Set := Letter_Set;
      Name_Body           : Character_Set := Alphanumeric_Set;
      Name_Separators     : Character_Set := To_Set ("-_");
      Header_Separator    : String := ":";
      Parameter_Separator : String := ",";
      Value_Separator     : String := "=";
      Open_Block          : String := "{";
      Close_Block         : String := "}")
      return Parser_Options
   is
      function "+" (X : String)
                    return Unbounded_String
                    renames To_Unbounded_String;

   begin
      return Parser_Options'(Expect_Header       => Expect_Header,
                             Name_Start          => Name_Start,
                             Name_Body           => Name_Body,
                             Name_Separators     => Name_Separators,
                             Header_Separator    => +Header_Separator,
                             Parameter_Separator => +Parameter_Separator,
                             Value_Separator     => +Value_Separator,
                             Open_Block          => +Open_Block,
                             Close_Block         => +Close_Block);
   end Config;


   procedure Check_Syntax (Item            : in out Parameter_Maps.Map;
                           Syntax          : Syntax_Descriptor;
                           On_Unknown_Name : Unknown_Name_Action)
   is
      use Syntax_Maps;

      procedure Check_Unknowns
      is
      begin
         for C in Item.Iterate loop
            declare
               Name  : constant Name_Type := Parameter_Maps.Key (C);
               Pos   : constant Syntax_Maps.Cursor := Syntax.S.Find (Name);
            begin
               if Pos = No_Element then
                  case On_Unknown_Name is
                  when OK =>
                     null;

                  when Die =>
                     raise Parsing_Error with "Unkwnon parameter";

                  when Default =>
                     if not Syntax.S.Is_Empty then
                        raise Parsing_Error with "Unkwnon parameter";
                     end if;
                  end case;
               end if;
            end;
         end loop;

      end Check_Unknowns;

      procedure Check_Missing is
      begin

         for C in Syntax.S.Iterate loop
            declare
               Name  : constant Name_Type := Key (C);
               Descr : constant Syntax_Entry := Element (C);
            begin
               if not Item.Contains (Name) then
                  case Descr.If_Missing is
                  when Die =>
                     raise Parsing_Error with "Missing parameter";

                  when Ignore =>
                     null;

                  when Use_Default =>
                     Item.Insert (Key      => Name,
                                  New_Item => Descr.Default.Element);
                  end case;
               end if;
            end;
         end loop;
      end Check_Missing;
   begin
      Check_Unknowns;
      Check_Missing;
   end Check_Syntax;

   -----------
   -- Parse --
   -----------

   function Parse
     (Input               : String;
                   Syntax              : Syntax_Descriptor := Empty_Syntax;
                   On_Unknown_Name     : Unknown_Name_Action := Default;
                   Options             : Parser_Options := Config)
      return Parsing_Result
   is
      use Ada.Characters.Handling;


      package Bounded_Names is
        new Ada.Strings.Bounded.Generic_Bounded_Length (Input'Length);

      use type Bounded_Names.Bounded_String;

      subtype Bounded_Name is Bounded_Names.Bounded_String;

      Cursor : Positive := Input'First;
      EOF    : constant Character := Character'Val (0);

      function At_EOF return Boolean
      is (Cursor > Input'Last);

      function Peek return Character
      is (if At_EOF then EOF else Input (Cursor));

      function Consume_Char return Character
      is

      begin
         if At_EOF then
            return EOF;
         else
            Cursor := Cursor + 1;
            return Input (Cursor - 1);
         end if;
      end Consume_Char;

      procedure Next_Char is
      begin
         Cursor := Cursor + 1;
      end Next_Char;

      procedure Skip_Spaces
        with Post => not Is_Space (Peek);

      procedure Skip_Spaces is
      begin
         while Is_Space (Peek) loop
            Next_Char;
         end loop;
      end Skip_Spaces;

      function Follows (What : Unbounded_String) return Boolean
      is
         R : constant String := Input (Cursor .. Input'Last);
         W : constant String := To_String (What);
      begin
         if R'Length < W'Length then
            return False;

         elsif R (R'First .. R'First + W'Length - 1) = W then
            return True;

         else
            return False;
         end if;
      end Follows;

      procedure Consume (What : Unbounded_String)
        with Pre => Follows (What);

      procedure Consume (What : Unbounded_String) is
      begin
         Cursor := Cursor + Length (What);
      end Consume;


      function Consume_If_Follows (What : Unbounded_String) return Boolean
      is
      begin
         if Follows (What) then
            Consume (What);
            return True;
         else
            return False;
         end if;
      end Consume_If_Follows;

      function Expect_Name return Bounded_Name
      is
         type State_Type is (Start, After_Start, After_Body, After_Separator);

         State : State_Type := Start;

         Result : Bounded_Name;
      begin

         Scanning_Loop :
         loop
            case State is
               when Start =>
                  if not Is_In (Peek, Options.Name_Start) then
                     raise Parsing_Error with "Invalid name begin";
                  end if;
                  State := After_Start;
                  Result := Result & Consume_Char;

               when After_Start | After_Body =>
                  if Is_In (Peek, Options.Name_Body) then
                     State := After_Body;
                     Result := Result & Consume_Char;

                  elsif Is_In (Peek, Options.Name_Separators) then
                     State := After_Separator;
                     Result := Result & Consume_Char;

                  else
                     exit Scanning_Loop;
                  end if;

               when After_Separator =>
                  if Is_In (Peek, Options.Name_Body) then
                     State := After_Body;
                     Result := Result & Consume_Char;
                  else
                     raise Parsing_Error with "Invalid character after separator";
                  end if;
            end case;
         end loop Scanning_Loop;

         return Result;
      end Expect_Name;

      function Expect_Value return Bounded_Name
      is
         Closure    : Unbounded_String;
         Is_A_Block : Boolean;
         Result     : Bounded_Name;
      begin
         Skip_Spaces;

         if Consume_If_Follows (Options.Open_Block) then
            Closure := Options.Close_Block;
            Is_A_Block := True;
         else
            Closure := Options.Parameter_Separator;
            Is_A_Block := False;
         end if;


         while not Follows (Closure) and not At_EOF loop
            Result := Result & Consume_Char;
         end loop;

         if Is_A_Block then
            if At_EOF then
               raise Parsing_Error with "Unterminated block";
            end if;

            pragma Assert (Follows (Closure));

            Consume (Closure);
         else
            Result := Bounded_Names.Trim (Result, Ada.Strings.Both);
         end if;

         return Result;
      end Expect_Value;

      ----------------------
      -- Expect_Parameter --
      ----------------------

      procedure Expect_Parameter (Result : in out Parameter_Maps.Map)
      is
         use Bounded_Names;

         Name   : Bounded_Name;
         Value  : Bounded_Name;
         Value_Given : Boolean;
      begin
         Name := Expect_Name;

         Skip_Spaces;

         if Consume_If_Follows (Options.Value_Separator) then
            Value := Expect_Value;
            Value_Given := True;
         else
            Value_Given := False;
         end if;

         Result.Insert
           (Key      =>
              To_Name (To_String (Name)),
            New_Item =>
              (if Value_Given then  To_Value (To_String (Value)) else  No_Value));

      end Expect_Parameter;

      function Parse_Headerless_List return Parameter_Maps.Map
      is
         Result : Parameter_Maps.Map := Parameter_Maps.Empty_Map;

      begin
         Skip_Spaces;

         if At_EOF then
            return Result;
         end if;

         loop
            Expect_Parameter (Result);

            Skip_Spaces;

            exit when At_EOF;

            if not Consume_If_Follows (Options.Parameter_Separator) then
               raise Parsing_Error with "Parameter separator expected";
            end if;

            Skip_Spaces;
         end loop;

         return Result;
      end Parse_Headerless_List;

      Name : Bounded_Name;
      Header_Given : Boolean;
      Result : Parsing_Result;
   begin
      Skip_Spaces;

      if At_EOF then
         -- It is acceptable that the line is empty: no parameter, nor
         -- header
         Header_Given := False;

      else
         -- If the string, it must always begin with a name.  We just do
         -- not know if it is a parameter of the header name.
         Name := Expect_Name;

         Skip_Spaces;

         if Consume_If_Follows (Options.Header_Separator) then
            -- The name is followed by a header separator: it is a header.
            -- Save it before parsing the parameter list

            Result.Header := Name_Holders.To_Holder (To_Name (Bounded_Names.To_String (Name)));
            Header_Given := True;
         else
            -- The name is not an header.  The easiest solution is to "rewind
            -- the tape" and call the function to parse the parameter list.
            -- Yes, it is not efficient, but usually this type of functions
            -- are called in contexts such as configuration file parsing
            -- or command line processing and tight efficiency is not
            -- necessary
            Cursor := Input'First;
            Header_Given := False;
         end if;

         Result.Parameters := Parse_Headerless_List;
      end if;

      if Options.Expect_Header = Yes and not Header_Given then
         raise Parsing_Error with "Header expected";

      elsif Options.Expect_Header = No and Header_Given then
         raise Parsing_Error with "Unexpected header";

      end if;


      Check_Syntax (Result.Parameters, Syntax, On_Unknown_Name);

      return Result;
   end Parse;

   -----------------------
   -- Parse_Syntax_Line --
   -----------------------

   function Parse_Syntax_Line (Syntax : String) return Syntax_Descriptor
   is
      pragma Unreferenced (Syntax);
   begin
      pragma Compile_Time_Warning (Standard.True, "Parse_syntax_line unimplemented");
      return raise Program_Error with "Unimplemented function Parse_Syntax_Line";
   end Parse_Syntax_Line;


   -----------
   -- Parse --
   -----------

   function Parse
     (Input               : String;
                   Syntax              : String;
                   On_Unknown_Name     : Unknown_Name_Action := Default;
                   Options             : Parser_Options := Config)
      return Parsing_Result
   is
   begin
      return Parse (Input           => Input,
                    Syntax          => Parse_Syntax_Line (Syntax),
                    On_Unknown_Name => On_Unknown_Name,
                    Options         => Options);
   end Parse;

end Config_String_Parsers;
