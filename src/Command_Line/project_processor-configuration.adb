with Ada.Containers.Vectors;
with Line_Parsers.Receivers.Multi_Receivers;
with Utilities;
with Plugins;
with Ada.Directories;
with Tokenize.Token_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Project_Processor.Configuration is
   package Call_Lists is
         new Ada.Containers.Vectors (Index_Type   => Positive,
                                     Element_Type => Processing_Call);

   package Multi_String_Receivers is
         new Line_Parsers.Receivers.Multi_Receivers (Line_Parsers.Receivers.String_Receiver);


   Input_File   : aliased Line_Parsers.Receivers.String_Receiver;
   Format       : aliased Line_Parsers.Receivers.String_Receiver;
   Processors   : aliased Multi_String_Receivers.Multi_Receiver;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Line_Parsers;

      Parser : Line_Parser := Create (Case_Sensitive => False);
   begin
      Add_Parameter (Parser,
                     Name       => "in,input",
                     If_Missing => Die,
                     Default    => "",
                     Handler    => Input_File'Access);

      Add_Parameter (Parser,
                     Name       => "p,proc,processor",
                     If_Missing => Ignore,
                     Default    => "",
                     Handler    => Processors'Access);


      Add_Parameter (Parser,
                     Name       => "f,fmt,format",
                     If_Missing => Ignore,
                     Default    => "",
                     Handler    => Format'Access);

      Parse_Command_Line (Parser, Slurp ("pjp.conf"));
   end Initialize;

   -----------------------------
   -- Parse_Plugin_Parameters --
   -----------------------------

   generic
      type Plugin_Name_Type is private;

      with function To_Name (X : String) return Plugin_Name_Type;
   procedure Parse_Plugin_Specs
         (Input                 : String;
          Plugin_Name           : out Plugin_Name_Type;
          Parameters            : out Plugins.Parameter_Map;
          Plugin_Name_Separator : Character := ':';
          Pair_Separator        : Character := ';';
          Value_Separator       : Character := '=');

   procedure Parse_Plugin_Specs
         (Input                 : String;
          Plugin_Name           : out Plugin_Name_Type;
          Parameters            : out Plugins.Parameter_Map;
          Plugin_Name_Separator : Character := ':';
          Pair_Separator        : Character := ';';
          Value_Separator       : Character := '=')
   is
      use Tokenize;

      procedure Parse_Params (Result : out Plugins.Parameter_Maps.Map;
                              Input  : Unbounded_String)
      is
         use Tokenize.Token_Vectors;

         Pairs : constant Token_Vectors.Vector :=
                   To_Vector (Split (To_Be_Splitted    => To_String (Input),
                                     Separator         => Pair_Separator,
                                     Collate_Separator => True));
      begin
         for P of Pairs loop
            declare
               --                 Name_And_Value : constant Token_Array :=
               --                                    Split (To_Be_Splitted    => P,
               --                                           Separator         => Value_Separator,
               --                                           Max_Token_Count   => 2);
               --
               --                 Name  : constant String := To_String (Name_And_Value (Name_And_Value'First));
               --                 Value : constant String :=
               --                           (if Name_And_Value'Length = 1
               --                            then ""
               --                            else To_String (Name_And_Value (Name_And_Value'First + 1)));
               Name  : Unbounded_String;
               Value : Unbounded_String;
            begin
               Head_And_Tail (To_Be_Splitted => P,
                              Separator      => Value_Separator,
                              Head           => Name,
                              Tail           => Value,
                              Trimming       => Both);

               Result.Include (Key      => To_String (Name),
                               New_Item => To_String (Value));

            end;
         end loop;
      end Parse_Params;

      function To_Name (X : Unbounded_String) return Plugin_Name_Type
      is (To_Name (To_String (X)));

      Head : Unbounded_String;
      Tail : Unbounded_String;
   begin
      Head_And_Tail (To_Be_Splitted => Input,
                     Separator      => Plugin_Name_Separator,
                     Head           => Head,
                     Tail           => Tail,
                     Trimming       => Both);

      Plugin_Name := To_Name (Head);

      Parse_Params (Parameters, Tail);
   end Parse_Plugin_Specs;

   -------------------
   -- For_All_Calls --
   -------------------



   procedure For_All_Calls
         (Callback : not null access procedure (Call : Processing_Call))
   is
      use Project_Processor.Processors;

      procedure Parse is
            new Parse_Plugin_Specs (Plugin_Name_Type => Processor_ID,
                                    To_Name          => To_Id);
      Data : Processing_Call := (Processing_Call'(Name       => <>,
                                                  Parameters => new Plugins.Parameter_Maps.Map));
   begin
      for Str of Processors.All_Values loop
         Parse (Input       => Str.Value,
                Plugin_Name => Data.Name,
                Parameters  => Data.Parameters.all);

         Callback (Data);
      end loop;
   end For_All_Calls;

   ----------------
   -- Input_Data --
   ----------------

   function Input_Data return String is
   begin
      return Utilities.Slurp (Input_File.Value);
   end Input_Data;

   ------------------
   -- Input_Format --
   ------------------

   function Input_Format return Parsers.Parser_ID
   is
      function Guess (Filename : String) return Parsers.Parser_ID
      is
         use Ada.Directories;
         use Parsers;

         Ext    : constant String := Extension (Filename);
         Result : constant Parser_ID := Find_Parser (Ext);
      begin
         if Result = No_Parser then
            raise Bad_Command_Line with "Unknown format";
         else
            return Result;
         end if;
      end Guess;
   begin
      if Format.Is_Set then
         return Parsers.Parser_ID (Format.Value);

      else
         return Guess (Input_File.Value);

      end if;
   end Input_Format;

   -----------------------
   -- Parser_Parameters --
   -----------------------

   function Parser_Parameters
         return Parsers.Parser_Parameter_Access
   is
      Result : constant Parsers.Parser_Parameter_Access := Plugins.Empty_Map;
   begin
      pragma Compile_Time_Warning (Standard.True, "Parser_Parameters unimplemented");
      --        raise Program_Error with "Unimplemented procedure Parser_Parameters";

      return Result;
   end Parser_Parameters;


end Project_Processor.Configuration;


