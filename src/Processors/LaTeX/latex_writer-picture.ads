with Ada.Strings.Fixed;

package Latex_Writer.Picture is
   subtype Latex_Slope is Integer range -6 .. 6;

   type Picture_Length is delta 0.01 range -10_000.0 .. 10_000.0;

   function "*" (X : Integer; Y : Picture_Length) return Picture_Length
   is (Picture_Length (X) * Y);

   function Pos (X, Y : Picture_Length) return String;


   function Put (X, Y : Picture_Length; What : String) return String
   is ("\put" & Pos (X, Y) & "{" & What & "}");

   function Line (Slope_X, Slope_Y : Latex_Slope; Length : Picture_Length) return String;

   function Arrow (Slope_X, Slope_Y : Latex_Slope; Length : Picture_Length) return String;

   function Hline (X, Y : Picture_Length; Length : Picture_Length) return String
   is (Put (X, Y, Line (1, 0, Length)));

   function Hline (Length : Picture_Length) return String
   is (Line (1, 0, Length));

   type Vertical_Direction is (Up, Down);

   function VLine (X, Y      : Picture_Length;
                   Length    : Picture_Length;
                   Direction : Vertical_Direction := Up) return String
   is (Put (X, Y, Line (Slope_X => 0,
                        Slope_Y => (case Direction is
                                       when Up   => 1,
                                       when Down => -1),
                        Length  => Length)));

   function VLine (Length    : Picture_Length;
                   Direction : Vertical_Direction := Up) return String
   is (Line (Slope_X => 0,
             Slope_Y => (case Direction is
                            when Up   => 1,
                            when Down => -1),
             Length  => Length));

   function Text (X, Y : Picture_Length; Content : String) return String;

   procedure Within_Picture
     (Output    : File_Access;
      Width     : Picture_Length;
      Heigth    : Picture_Length;
      Callback  : access procedure (Output : File_Access));
private
   function Chop (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Image (X : Integer) return String
   is (Chop (Integer'Image (X)));

   function Image (X : Picture_Length) return String
   is (Chop (Picture_Length'Image (X)));


   function Pos (X, Y : Picture_Length) return String
   is ("(" & Image (X) & "," & Image (Y) & ")");

   function Slope (X, Y : Latex_Slope) return String
   is ("(" & Image (X) & "," & Image (Y) & ")");

   function Line (Slope_X, Slope_Y : Latex_Slope; Length : Picture_Length) return String
   is ("\line" & Slope (Slope_X, Slope_Y) & "{"  & Chop (Picture_Length'Image (Length)) & "}");

   function Arrow (Slope_X, Slope_Y : Latex_Slope; Length : Picture_Length) return String
   is ("\vector" & Slope (Slope_X, Slope_Y) & "{"  & Chop (Picture_Length'Image (Length)) & "}");

   function Text (X, Y : Picture_Length; Content : String) return String
   is (Put (X, Y, "{" & Content & "}"));



end Latex_Writer.Picture;
