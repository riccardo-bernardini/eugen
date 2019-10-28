with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package EU_Projects.Times is

   type Instant is private;
   type Duration is private;
   type Interval is private;

   type Time_Type is (Instant_Value, Duration_Value);


   Earliest_Istant : constant Instant;
   Latest_Istant   : constant Instant;
   No_Instant      : constant Instant;
   To_Be_Decided   : constant Instant;

   function To_Instant (Month : Natural; Week : Natural := 0)
                        return Instant;

   function Months (Item : Instant)
                    return Integer
     with Pre => Item /= To_Be_Decided;


   type Unit_Type is (Months, Weeks);

   type Time_Formatter is tagged private;
   Default_Formatter : constant Time_Formatter;

   type Formatting_Option is (To_Be_Decided_Image);

   function Create return Time_Formatter
   is (Default_Formatter);

   function Set_Option (Formatter : Time_Formatter;
                        Option    : Formatting_Option;
                        Value     : String)
                        return Time_Formatter;

   function Image (Formatter  : Time_Formatter;
                   Item       : Instant;
                   Unit       : Unit_Type := Months;
                   Write_Unit : Boolean := False) return String;

   function Image (Item       : Instant;
                   Unit       : Unit_Type := Months;
                   Write_Unit : Boolean := False) return String
   is (Default_Formatter.Image (Item, Unit, Write_Unit));

   function To_Duration (Month : Natural; Week : Natural := 0)
                         return Duration;

   function To_Duration (X : Instant) return Duration;

   function To_Months (Item : Duration)
                       return Integer;

   function To_Interval (From, To : Instant) return Interval;
   pragma Precondition (From <= To);

   function Min (L, R : Instant) return Instant;
   function Max (L, R : Instant) return Instant;

   function "=" (L, R : Instant) return Boolean;
   function "<" (L, R : Instant) return Boolean;
   function "<=" (L, R : Instant) return Boolean;
   function ">" (L, R : Instant) return Boolean;
   function ">=" (L, R : Instant) return Boolean;

   function "-" (L, R : Instant) return Duration;
   function "+" (L : Instant; R : Duration) return Instant;
   function "-" (L : Instant; R : Duration) return Instant;
   function "+" (L : Duration; R : Instant) return Instant;

   function "*" (L : Duration; R : Float) return Duration;


   function Start_Of (Item : Interval) return Instant;
   function End_Of (Item : Interval) return Instant;
   function Duration_Of (Item : Interval) return Duration;

   function Is_Empty (X : Interval) return Boolean;
   function "<" (L, R : Interval) return Boolean;
   function "and" (L, R : Interval) return Interval;
   function "or" (L, R : Interval) return Interval;

   Empty_Interval : constant Interval;
private
   subtype Scalar_Base is Integer;

   type Scalar_Type (Tbd : Boolean := False) is
      record
         case Tbd is
            when True =>
               null;

            when False =>
               N : Scalar_Base;
         end case;
      end record;

   TBD    : constant Scalar_Type := Scalar_Type'(Tbd => True);
   Untrue : constant Scalar_Type := Scalar_Type'(Tbd => False, N => 0);
   True   : constant Scalar_Type := Scalar_Type'(Tbd => False, N => 1);

   type Scalar_Array is array (Positive range <>) of Scalar_Type;


   function "+" (X, Y : Scalar_Type) return Scalar_Type
   is (if (X.Tbd or Y.Tbd) then
          Tbd
       else
         (Tbd => False, N => X.N + Y.N));

   function "+" (X : Scalar_Type) return Scalar_Type
   is (X);

   function "-" (X : Scalar_Type) return Scalar_Type
   is (if X.Tbd then
          Tbd
       else
         (Tbd => False, N => -X.N));

   function "-" (X, Y : Scalar_Type) return Scalar_Type
   is (if (X.Tbd or Y.Tbd) then
          Tbd
       else
         (Tbd => False, N => X.N - Y.N));

   function "*" (X, Y : Scalar_Type) return Scalar_Type
   is (if (X.Tbd or Y.Tbd) then
          Tbd
       else
         (Tbd => False, N => X.N * Y.N));

   function "/" (X, Y : Scalar_Type) return Scalar_Type
   is (if (X.Tbd or Y.Tbd) then
          Tbd
       else
         (Tbd => False, N => X.N / Y.N));

   function Min (X, Y : Scalar_Type) return Scalar_Type
   is (if (X.Tbd or Y.Tbd) then
          Tbd
       else
         (Tbd => False, N => Scalar_Base'Min (X.N, Y.N)));

   function Max (X, Y : Scalar_Type) return Scalar_Type
   is (if (X.Tbd or Y.Tbd) then
          Tbd
       else
         (Tbd => False, N => Scalar_Base'Max (X.N, Y.N)));

   function "abs" (X : Scalar_Type) return Scalar_Type
   is (if X.Tbd then
          Tbd
       else
         (Tbd => False, N => abs X.N));


   function Image (Item : Scalar_Type) return String
   is (if Item.Tbd then
          "TBD"
       else
          Integer'Image (Item.N));

   type Instant is new Scalar_Type;
   type Duration is new Scalar_Type;

   ---------
   -- Min --
   ---------

   function Min (L, R : Instant) return Instant
   is (Instant (Min (Scalar_Type (L), Scalar_Type (R))));

   function Max (L, R : Instant) return Instant
   is (Instant (Max (Scalar_Type (L), Scalar_Type (R))));

   type Interval is
      record
         Start : Instant;
         Stop  : Instant;
      end record;

   function Scalar (X : Scalar_Base) return Instant
   is (Instant'(False, X));

   Empty_Interval : constant Interval := (Start => Scalar (1), Stop => Scalar (0));

   Earliest_Istant : constant Instant := Scalar (Scalar_Base'First);
   Latest_Istant   : constant Instant := Scalar (Scalar_Base'Last - 1);
   No_Instant      : constant Instant := Scalar (Scalar_Base'Last);
   To_Be_Decided   : constant Instant := Instant (Tbd);

   type Time_Formatter is tagged
      record
         Tbd_Image : Unbounded_String;
      end record;

   Default_Formatter : constant Time_Formatter :=
                         Time_Formatter'(Tbd_Image => To_Unbounded_String ("TBD"));
end EU_Projects.Times;
