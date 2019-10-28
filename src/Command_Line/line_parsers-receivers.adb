pragma Ada_2012;
package body Line_Parsers.Receivers is

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
         (Handler  : in out String_Receiver;
          Name     : String;
          Value    : String;
          Position : Natural)
   is
      pragma Unreferenced (Name, Position);
   begin
      Handler.Value := To_Unbounded_String (Value);
      Handler.Set := True;
   end Receive;

   -------------
   -- Receive --
   -------------

   procedure Receive
         (Handler  : in out Integer_Receiver;
          Name     : String;
          Value    : String;
          Position : Natural)
   is
      pragma Unreferenced (Name, Position);
   begin
      Handler.Value := Integer'Value (Value);
      Handler.Set := True;
   end Receive;

   -------------
   -- Receive --
   -------------

   procedure Receive
         (Handler  : in out Float_Receiver;
          Name     : String;
          Value    : String;
          Position : Natural)
   is
      pragma Unreferenced (Name, Position);
   begin
      Handler.Value := Float'Value (Value);
      Handler.Set := True;
   end Receive;



end Line_Parsers.Receivers;
