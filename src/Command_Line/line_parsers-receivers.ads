--
-- This package provides few handlers for common types: strings,
-- integers and float.  A separate package provides a generic
-- handler for enumerative types.
--
package Line_Parsers.Receivers is
   type String_Receiver is new Abstract_Parameter_Handler with private;

   overriding
   function Is_Set(Handler: String_Receiver) return Boolean;

   overriding
   procedure Receive (Handler : in out String_Receiver;
                      Name    : String;
                      Value   : String;
                      Position : Natural);
   overriding
   function Reusable(Handler: String_Receiver) return Boolean;

   function Value (Handler : String_Receiver) return String
         with Pre => Handler.Is_Set;

   type Integer_Receiver is new Abstract_Parameter_Handler with private;

   overriding
   function Is_Set (Handler : integer_Receiver) return Boolean;

   overriding
   procedure Receive (Handler : in out Integer_Receiver;
                      Name    : String;
                      Value   : String;
                      Position : Natural);

   overriding
   function Reusable (Handler : Integer_Receiver) return Boolean;

   function Get (Handler : Integer_Receiver) return Integer
         with Pre => Handler.Is_Set;


   type Float_Receiver is new Abstract_Parameter_Handler with private;

   overriding
   function Is_Set (Handler : Float_Receiver) return Boolean;

   procedure Receive (Handler : in out Float_Receiver;
                      Name    : String;
                      Value   : String;
                      Position : Natural);

   function Get (Handler : Float_Receiver) return Float
         with Pre => Handler.Is_Set;

   overriding
   function Reusable(Handler: Float_Receiver) return Boolean;

private
   type String_Receiver is new Abstract_Parameter_Handler with
      record
         Set : Boolean := False;
         Value : Unbounded_String;
      end record;

   function Is_Set (Handler : String_Receiver) return Boolean
   is (Handler.Set);

   function Value (Handler : String_Receiver) return String
   is (To_String (Handler.Value));

   function Reusable(Handler: String_Receiver) return Boolean
   is (False);


   type Integer_Receiver is new Abstract_Parameter_Handler with
      record
         Set : Boolean := False;
         Value : Integer;
      end record;



   function Is_Set (Handler : Integer_Receiver) return Boolean
   is (Handler.Set);

   function Get (Handler : Integer_Receiver) return Integer
   is (Handler.Value);

   function Reusable(Handler: Integer_Receiver) return Boolean
   is (False);

   type Float_Receiver is new Abstract_Parameter_Handler with
      record
         Set : Boolean := False;
         Value : Float;
      end record;


   function Is_Set (Handler : Float_Receiver) return Boolean
   is (Handler.Set);

   function Get (Handler : Float_Receiver) return Float
   is (Handler.Value);

   function Reusable(Handler: Float_Receiver) return Boolean
   is (False);
end Line_Parsers.Receivers;
