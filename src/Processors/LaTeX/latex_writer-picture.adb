pragma Ada_2012;
package body Latex_Writer.Picture is

    --------------------
   -- Within_Picture --
   --------------------

   procedure Within_Picture
     (Output    : File_Access;
      Width     : Picture_Length;
      Heigth    : Picture_Length;
      Callback  : access procedure (Output : File_Access))
   is
   begin
      Put (Output.all, "\begin{picture}" & Picture.Pos (Width, Heigth));

      New_Line (Output.all);

      Callback (Output);

      New_Line (Output.all);
      Put_Line (Output.all, "\end{picture}");
   end Within_Picture;


end Latex_Writer.Picture;
