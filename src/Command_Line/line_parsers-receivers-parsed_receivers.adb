pragma Ada_2012;
package body Line_Parsers.Receivers.Parsed_Receivers is

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
      Handler.Value := Parse (Value);
      Handler.Set := True;
   end Receive;

end Line_Parsers.Receivers.Parsed_Receivers;
