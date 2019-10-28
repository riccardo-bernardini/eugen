--
-- This package provides a generic handler for a "parsed" type, that is,
-- a type that has a function "Parse" to map a string to a value.
--
generic
   type Value_Type is private;

   with function Parse (Item : String) return Value_Type is <>;
package Line_Parsers.Receivers.Parsed_Receivers is
   type Receiver_Type is new Abstract_Parameter_Handler with private;


   function Is_Set (Handler : Receiver_Type) return Boolean;

   procedure Receive (Handler  : in out Receiver_Type;
                      Name     : String;
                      Value    : String;
                      Position : Natural);

   function Reusable (Handler : Receiver_Type) return Boolean;

   function Get (Handler : Receiver_Type) return Value_Type
         with Pre => Handler.Is_Set;


private
   type Receiver_Type is new Abstract_Parameter_Handler with
      record
         Value : Value_Type;
         Set   : Boolean := False;
      end record;

   function Get (Handler : Receiver_Type) return Value_Type
   is (Handler.Value);


   function Reusable (Handler : Receiver_Type) return Boolean
   is (False);


   function Is_Set (Handler : Receiver_Type) return Boolean
   is (Handler.Set);

end Line_Parsers.Receivers.Parsed_Receivers;
