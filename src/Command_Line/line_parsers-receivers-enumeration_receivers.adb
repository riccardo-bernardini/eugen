pragma Ada_2012;
package body Line_Parsers.Receivers.Enumeration_Receivers is

   -------------
   -- Receive --
   -------------

   procedure Receive
         (Handler  : in out Receiver_Type;
          Name     : String;
          Value    : String;
          Position : Natural)
   is
      pragma Unreferenced (Name, Position);
   begin
      Handler.Value := Value_Type'Value (Value);
      Handler.Set := True;
   end Receive;
end Line_Parsers.Receivers.Enumeration_Receivers;
