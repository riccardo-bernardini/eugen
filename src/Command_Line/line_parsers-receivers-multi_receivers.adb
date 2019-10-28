pragma Ada_2012;
package body Line_Parsers.Receivers.Multi_Receivers is

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Handler  : in out Multi_Receiver;
      Name     : String;
      Value    : String;
      Position : Natural)
   is
      Basic : Basic_Receiver;
   begin
      Basic.Receive (Name, Value, Position);
      Handler.Values.Append (Basic);
      Handler.Set := True;
   end Receive;

end Line_Parsers.Receivers.Multi_Receivers;
