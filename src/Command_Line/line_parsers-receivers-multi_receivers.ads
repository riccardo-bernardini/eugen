--
-- This package provides a "multi receiver," that is a handler that is able
-- to store many instances of a basic handler.  After the parsing it
-- can be seen as a vector with a length and a way to access the n-th element.
--
with Ada.Containers.Vectors;

generic
   type Basic_Receiver is new Abstract_Parameter_Handler with private;
package Line_Parsers.Receivers.Multi_Receivers is
   type Multi_Receiver is new Abstract_Parameter_Handler with private;

   overriding
   function Is_Set (Handler : Multi_Receiver) return Boolean;

   overriding
   function Reusable (Handler : Multi_Receiver) return Boolean;

   overriding
   procedure Receive (Handler  : in out Multi_Receiver;
                      Name     : String;
                      Value    : String;
                      Position : Natural);


   -- Return the number of times Receive was called
   function Length (Handler : Multi_Receiver) return Ada.Containers.Count_Type;


   function Element (Handler :  Multi_Receiver;
                     Pos     : Positive)
                     return Basic_Receiver;

   package Value_Vectors is
         new Ada.Containers.Vectors (Index_Type   => Positive,
                                     Element_Type => Basic_Receiver);

   function All_Values (Handler : Multi_Receiver) return Value_Vectors.Vector;
private
   use type Ada.Containers.Count_Type;

   type Multi_Receiver is new Abstract_Parameter_Handler with
      record
         Values : Value_Vectors.Vector;
         Set    : Boolean := False;
      end record;


   function Is_Set (Handler : Multi_Receiver) return Boolean
   is (Handler.Length > 0);

   function All_Values (Handler : Multi_Receiver) return Value_Vectors.Vector
   is (Handler.Values);

   function Reusable (Handler : Multi_Receiver) return Boolean
   is (True);

   function Length (Handler : Multi_Receiver) return Ada.Containers.Count_Type
   is (Handler.Values.Length);

   function Element (Handler :  Multi_Receiver; Pos     : Positive)
                     return Basic_Receiver
   is (Handler.Values (Pos));
end Line_Parsers.Receivers.Multi_Receivers;
