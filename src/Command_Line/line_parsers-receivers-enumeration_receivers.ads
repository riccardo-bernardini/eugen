--
-- This package provides handlers for enumerative types.  Since the
-- type is not fixed in advance, the package needs to be generic.
--
generic
   type Value_Type is (<>);
package Line_Parsers.Receivers.Enumeration_Receivers is
   type Receiver_Type is new Abstract_Parameter_Handler with private;


   function Is_Set (Handler : Receiver_Type) return Boolean;

   overriding
   procedure Receive (Handler : in out Receiver_Type;
                      Name    : String;
                      Value   : String;
                      Position : Natural);

   function Get (Handler : Receiver_Type) return Value_Type
         with Pre => Handler.Is_Set;

   overriding function Reusable (Handler : Receiver_Type) return Boolean;

private
   type Receiver_Type is new Abstract_Parameter_Handler with
      record
         Value : Value_Type;
         Set   : Boolean := False;
      end record;


   function Is_Set (Handler : Receiver_Type) return Boolean
   is (Handler.Set);

   function Reusable (Handler : Receiver_Type) return Boolean
   is (False);

   function Get (Handler : Receiver_Type) return Value_Type
   is (Handler.Value);

end Line_Parsers.Receivers.Enumeration_Receivers;
