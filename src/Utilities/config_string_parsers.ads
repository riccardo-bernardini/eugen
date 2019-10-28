with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Maps.Constants;    use Ada.Strings.Maps.Constants, Ada.Strings.Maps;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

--
--
-- Syntax:
--
--   line      = [header] parameter ("," parameter)*
--   header    = name ":"
--   parameter = name ["=" value]
--   value     = "{" [^}]* "}" | [^{][^,]*
--   name      = begin body* (S body+)*
--
generic
   type Name_Type (<>) is private;
   type Value_Type (<>) is private;

   No_Value    :  Value_Type;

   with function "<" (X, Y : Name_Type) return Boolean is <>;
   with function To_Name (X : String) return Name_Type;
   with function To_Value (X : String) return Value_Type;
package Config_String_Parsers is
   -- This type is used to describe the parameters that we expect,
   -- if they are mandatory or not and any default value.
   type Syntax_Descriptor is private;

   Empty_Syntax : constant Syntax_Descriptor;

   -- What to do when a mandatory parameter is missing
   type Missing_Action is (Die,           -- raise Parsing_Error
                           Ignore,        -- OK, the parameter is optional
                           Use_Default);  -- use a default value

   -- Add a new parameter to the descriptor.  Note that the precondition
   -- requires that Default is specified only if If_Missing = Use_Default
   procedure Add_Parameter_Syntax (Syntax         : in out Syntax_Descriptor;
                                   Parameter_Name : Name_Type;
                                   If_Missing     : Missing_Action;
                                   Default        : Value_Type := No_Value)
     with
       Pre => (if Default /= No_Value then If_Missing = Use_Default);

   package Parameter_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Name_Type,
                                                 Element_Type => Value_Type);

   -- This type represents the result of a parsing.  It holds the
   -- header value (if present) and the parameter list as a map
   -- parameter -> value
   type Parsing_Result is  private;

   -- Return true if an header was given
   function Has_Header (H : Parsing_Result) return Boolean;

   -- Return the name used for the header
   function Header (H : Parsing_Result) return Name_Type
     with Pre => Has_Header (H);

   -- Return the parameter list
   function Parameters (H : Parsing_Result) return Parameter_Maps.Map;


   -- This type is used to hold several "configuration options" of the
   -- parser, for example: the separator after the header, the separator
   -- between parameters, etc.
   type Parser_Options (<>) is private;

   -- Is the header required?
   type Header_Expected is
     (Yes,     -- The header is mandatory
      No,      -- The header is prohibited
      Maybe);  -- The header is optional

   -- What to do when a parameter not included in the syntax is found?
   type Unknown_Name_Action is
     (OK,        -- accept it
      Die,       -- raise Parsing_Error
      Default);  -- Like OK if no syntax is specified, otherwise Die

   -- Create a Parser_Options specifier. Note that all parameters are
   -- optional, so that only the parameters that need to change need to
   -- be specified.
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
      return Parser_Options;

   function Parse
     (Input               : String;
      Syntax              : Syntax_Descriptor := Empty_Syntax;
      On_Unknown_Name     : Unknown_Name_Action := Default;
      Options             : Parser_Options := Config)
      return Parsing_Result;

   function Parse
     (Input               : String;
      Syntax              : String;
      On_Unknown_Name     : Unknown_Name_Action := Default;
      Options             : Parser_Options := Config)
      return Parsing_Result
     with Pre => False;
   pragma Compile_Time_Warning (Standard.True, "Parse version unimplemented");


   Parsing_Error : exception;
private
   package Value_Holders is
     new Ada.Containers.Indefinite_Holders (Value_Type);

   package Name_Holders is
     new Ada.Containers.Indefinite_Holders (Name_Type);

   --     overriding function Copy (X : Parameter_Holder) return Parameter_Holder;

   type Parsing_Result is
      record
         Parameters : Parameter_Maps.Map;
         Header     : Name_Holders.Holder;
      end record;

   function Has_Header (H : Parsing_Result) return Boolean
   is (not H.Header.Is_Empty);

   function Header (H : Parsing_Result) return Name_Type
   is (H.Header.Element);

   function Parameters (H : Parsing_Result) return Parameter_Maps.Map
   is (H.Parameters);


   type Parser_Options is
      record
         Expect_Header       : Header_Expected;
         Name_Start          : Character_Set;
         Name_Body           : Character_Set;
         Name_Separators     : Character_Set;
         Header_Separator    : Unbounded_String;
         Parameter_Separator : Unbounded_String;
         Value_Separator     : Unbounded_String;
         Open_Block          : Unbounded_String;
         Close_Block         : Unbounded_String;
      end record;



   type Syntax_Entry is
      record
         If_Missing : Missing_Action;
         Default    : Value_Holders.Holder;
      end record;

   package Syntax_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Name_Type,
                                                 Element_Type => Syntax_Entry);

   type Syntax_Descriptor is
      record
         S : Syntax_Maps.Map;
      end record;

   Empty_Syntax : constant Syntax_Descriptor := (S => Syntax_Maps.Empty_Map);
end Config_String_Parsers;
