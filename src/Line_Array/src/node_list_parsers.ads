with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Line_Arrays;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--
-- This package parses a file as a list of "nodes."  Basically,
-- a "node" has the following properties
--
--    * A node "class"
--    * A list of (attribute, value) pairs
--    * A descriptive text
--
--  A node it is stored like
--
-- [wp]
-- name : zorro
-- label : foo
-- viva la pappa col pomodoro.
--
-- Attribute lines have the form "key : value" where the key must begin
-- in column 1.  Attributes come after the header with the class name and
-- before the description.  Empty lines before or after the description
-- and spaces before and after the attribute value can be trimmed.
--
-- Lines begining with # are comments and ignored
--
-- The syntax is something like
--
--   file      := node+
--   node      := header attribute* text-line*
--   header    := ^S* '[' S* id S*  ']' S*$
--   attribute := ^id S* ':' .*$
--   text-line := does not match neither header, nor attribute
--
--

generic
   type Node_Class is (<>);

   with package Bounded_Identifiers is
     new Ada.Strings.Bounded.Generic_Bounded_Length (<>);
package Node_List_Parsers is
   type Node_Type is tagged private
     with Constant_Indexing => Value;

   type Node_List is array (Positive range <>) of Node_Type;



   function Is_Valid_Id (X : String) return Boolean
   is (for all K of X =>
         (K in 'a' .. 'z') or K = '-');

   --
   --     package Id_Strings is
   --           new Ada.Strings.Bounded.Generic_Bounded_Length (Max_ID_Length);
   --
   --     use Id_Strings;

   subtype Id is Bounded_Identifiers.Bounded_String;
   use type Bounded_Identifiers.Bounded_String;
   --     Id_Strings.Bounded_String
   --           with Dynamic_Predicate => (Is_Valid_Id (To_String (Id)));


   function "+" (X : String) return Id
   is (Bounded_Identifiers.To_Bounded_String (X));

   function To_ID (X : String) return Id
   is (Bounded_Identifiers.To_Bounded_String (X));

   function Class (Item : Node_Type) return Node_Class;

   function Description (Item : Node_Type) return Line_Arrays.Line_Array;

   function Description (Item : Node_Type) return String
   is (Line_Arrays.Join (Item.Description));

   function Contains (Item : Node_Type;
                      Key  : Id)
                      return Boolean;

   function Value (Item : Node_Type;
                   Key  : Id)
                   return String;

   function Value (Item : Node_Type;
                   Key  : String)
                   return String
   is (Item.Value (To_ID (Key)));

   function Value (Item    : Node_Type;
                   Key     : Id;
                   Default : String) return String;


   package Name_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Id,
                                                 Element_Type => Node_Class);

   type Trimming_Action is (Head, Tail, Both, None);

   function Parse (Input         : Line_Arrays.Line_Array;
                   Names         : Name_Maps.Map := Name_Maps.Empty_Map;
                   Trimming      : Trimming_Action := Both;
                   Line_Trimming : Trimming_Action := Both)
                   return Node_List;

   procedure Dump (Item : Node_List);

   type Node_Scanner (<>) is private;

   function To_Scanner (X : Node_List) return Node_Scanner;

   function End_Of_List (X : Node_Scanner) return Boolean;

   function Peek (X : Node_Scanner) return Node_Type
     with Pre => not End_Of_List (X);

   function Peek_Class (X : Node_Scanner) return Node_Class
   is (Peek (X).Class)
     with Pre => not End_Of_List (X);
   -- Syntactic sugar, but very useful

   function Class_Is (X : Node_Scanner; Cl : Node_Class) return Boolean
   is ((not End_Of_List (X)) and then (Peek_Class (X) = Cl));
   -- Syntactic sugar again, very convenient since it includes also the
   -- end-of-data case

   procedure Next (X : in out Node_Scanner);

   procedure Prev (X : in out Node_Scanner);
   type Attribute_Check is private;

   function Mandatory (Name : String) return Attribute_Check;

   function Default (Name : String; Default : String) return Attribute_Check;

   function Alternative (Spec : String) return Attribute_Check;

   function Enumerative (Spec           : String;
                         Allowed_Values : String;
                         Default        : String := "";
                         Case_Sensitive : Boolean := False)
                         return Attribute_Check;

   generic
      type Enumerative_Type is (<>);
   function Generic_Enumerative (Spec    : String;
                                 Default : String := "")
                                 return Attribute_Check;

   type Attribute_Check_Array is array (Positive range <>) of Attribute_Check;

   type Attribute_Checker (<>) is private;

   function Create (Checks : Attribute_Check_Array) return Attribute_Checker;

   function "+" (X, Y : Attribute_Checker) return Attribute_Checker;

   function "+" (X : Attribute_Check;
                 Y : Attribute_Check)
                 return Attribute_Checker;

   function "+" (X : Attribute_Checker;
                 Y : Attribute_Check)
                 return Attribute_Checker;

   procedure Check (Checker : Attribute_Checker;
                    Node    : in out Node_Type);
   Missing_Mandatory     : exception;
   Missing_Alternative   : exception;
   Duplicate_Alternative : exception;
   Bad_Enumerative       : exception;
private
   package Key_Value_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Id,
                                                 Element_Type => String);
   type Node_Type is tagged
      record
         Class       : Node_Class;
         Attributes  : Key_Value_Maps.Map;
         Description : Line_Arrays.Line_Array;
      end record;

   function Class (Item : Node_Type) return Node_Class
   is (Item.Class);

   function Description (Item : Node_Type) return Line_Arrays.Line_Array
   is (Item.Description);

   function Contains (Item : Node_Type;
                      Key  : Id)
                      return Boolean
   is (Item.Attributes.Contains (Key));

   function Value (Item : Node_Type;
                   Key  : Id)
                   return String
   is (Item.Attributes.Element (Key));

   function Value (Item    : Node_Type;
                   Key     : Id;
                   Default : String) return String
   is (if Item.Contains (Key) then Item.Value (Key) else Default);

   type Node_Scanner (Length : Positive) is
      record
         Nodes  : Node_List (1 .. Length);
         Cursor : Positive;
      end record;

   function To_Scanner (X : Node_List) return Node_Scanner
   is (Node_Scanner'(Length => X'Length,
                     Nodes  => X,
                     Cursor => 1));

   function End_Of_List (X : Node_Scanner) return Boolean
   is (X.Cursor > X.Nodes'Last);

   function Peek (X : Node_Scanner) return Node_Type
   is (X.Nodes (X.Cursor));

   package ID_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Id);

   type Check_Class is (Mandatory_Class,
                        Default_Class,
                        Alternative_Class,
                        Enumerative_Class);

   type Attribute_Check (Class : Check_Class := Mandatory_Class) is
      record
         case Class is
            when Mandatory_Class =>
               Attribute : Id;

            when Default_Class =>
               Key   : Id;
               Value : Unbounded_String;

            when Alternative_Class =>
               Alternatives : Id_Vectors.Vector;

            when Enumerative_Class =>
               Attr           : Id;
               Allowed        : ID_Vectors.Vector;
               Default        : Unbounded_String;
               Case_Sensitive : Boolean;
         end case;
      end record;

   function Mandatory (Name : String) return Attribute_Check
   is (Attribute_Check'(Class => Mandatory_Class,
                        Attribute => To_ID (Name)));

   function Default (Name : String; Default : String) return Attribute_Check
   is (Attribute_Check'(Class => Default_Class,
                        Key       => To_Id (Name),
                        Value     => To_Unbounded_String (Default)));

   type Attribute_Checker (Length : Positive) is
      record
         Checks : Attribute_Check_Array (1 .. Length);
      end record;

   function Create (Checks : Attribute_Check_Array) return Attribute_Checker
   is (Attribute_Checker'(Length => Checks'Length,
                          Checks => Checks));

   function "+" (X, Y : Attribute_Checker) return Attribute_Checker
   is (Attribute_Checker'(Length => X.Length + Y.Length,
                          Checks => X.Checks & Y.Checks));


   function "+" (X : Attribute_Check; Y : Attribute_Check)
                 return Attribute_Checker
   is (Attribute_Checker'(Length => 2,
                          Checks => (1 => X, 2 => Y)));

   function "+" (X : Attribute_Checker; Y : Attribute_Check) return Attribute_Checker
   is (X + Create ((1 => Y)));
end Node_List_Parsers;
