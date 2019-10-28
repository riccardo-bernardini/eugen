package body EU_Projects.Times is



   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (X : Instant) return Duration is
   begin
      return Duration(X);
   end To_Duration;

   function Is_Empty (X : Interval) return Boolean
   is
   begin
      return X.Start > X.Stop;
   end Is_Empty;

   function "<" (L, R : Interval) return Boolean
   is
   begin
      return (not Is_Empty(L)) and then (not Is_Empty(R)) and then R.Stop < L.Start;
   end "<";

   function "and" (L, R : Interval) return Interval
   is
   begin
      if Is_Empty (L) or Is_Empty (R) then
         return Empty_Interval;
      else
         return Interval'(Start => Max (L.Start, R.Start),
                          Stop  => Min (L.Stop, R.Stop));
      end if;
   end "and";

   function "or" (L, R : Interval) return Interval
   is
   begin
      if Is_Empty (L) then
         return R;
      elsif Is_Empty (R) then
         return L;
      elsif Is_Empty (L and R) then
         raise Constraint_Error;
      else
         return Interval'(Start => Min (L.Start, R.Start),
                          Stop  => Max (L.Stop, R.Stop));
      end if;
   end "or";

   ---------
   -- "<" --
   ---------

   function "=" (L, R : Instant) return Boolean
   is (if L.Tbd /= R.Tbd then
          False
       elsif L.Tbd then
          Standard.True
       else
          L.N = R.N);


   function "<" (L, R : Instant) return Boolean
   is (if L.Tbd then
          False
       elsif R.Tbd then
          Standard.True
       else
          L.N < R.N);

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Instant) return Boolean
   is (L = R or else L < R);

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Instant) return Boolean
   is (R < L);

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Instant) return Boolean
   is (R <= L);

   ----------------
   -- To_Instant --
   ----------------

   function To_Instant
     (Month : Natural;
      Week : Natural := 0)
      return Instant
   is
   begin
      return Scalar (4 * Integer (Month) + Integer (Week));
   end To_Instant;

   ---------------
   -- To_Months --
   ---------------

   function Months (Item : Instant)
                    return Integer
   is (if Item.Tbd then
          raise Constraint_Error
       else
          Item.N / 4);

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration
     (Month : Natural;
      Week : Natural := 0)
      return Duration
   is
   begin
      return Duration (Scalar (4 * Integer (Month) + Integer (Week)));
   end To_Duration;

   ---------------
   -- To_Months --
   ---------------

   function To_Months
     (Item : Duration)
      return Integer
   is (if Item.Tbd then
          raise Constraint_Error
       else
          Item.N / 4);

   -----------------
   -- To_Interval --
   -----------------

   function To_Interval (From, To : Instant) return Interval is
   begin
      return Interval'(Start => From,
                       Stop  => To);
   end To_Interval;

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Instant) return Duration is
   begin
      return Duration(abs (Scalar_Type (L)-Scalar_Type (R)));
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (L : Instant; R : Duration) return Instant is
   begin
      return Instant (Scalar_Type (L) + Scalar_Type (R));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Instant; R : Duration) return Instant is
   begin
      return Instant (Scalar_Type (L) - Scalar_Type (R));
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (L : Duration; R : Instant) return Instant is
   begin
      return Instant (L) + R;
   end "+";

   ---------
   -- "*" --
   ---------

   function "*" (L : Duration; R : Float) return Duration is
   begin
      return L * Duration (Scalar (Integer (R)));
   end "*";

   --------------
   -- Start_Of --
   --------------

   function Start_Of (Item : Interval) return Instant is
   begin
      return Item.Start;
   end Start_Of;

   ------------
   -- End_Of --
   ------------

   function End_Of (Item : Interval) return Instant is
   begin
      return Item.Stop;
   end End_Of;

   -----------------
   -- Duration_Of --
   -----------------

   function Duration_Of (Item : Interval) return Duration is
   begin
      return Item.Stop - Item.Start;
   end Duration_Of;

   ----------------
   -- Set_Option --
   ----------------

   function Set_Option (Formatter : Time_Formatter;
                        Option    : Formatting_Option;
                        Value     : String)
                        return Time_Formatter
   is
      Result : Time_Formatter := Formatter;
   begin
      case Option is
         when To_Be_Decided_Image =>
            Result.Tbd_Image := To_Unbounded_String (Value);
      end case;

      return Result;
   end Set_Option;


   -----------
   -- Image --
   -----------

   function Image (Formatter  : Time_Formatter;
                   Item       : Instant;
                   Unit       : Unit_Type := Months;
                   Write_Unit : Boolean := False) return String
   is
   begin
      if Item.Tbd then
         return To_String (Formatter.Tbd_Image);
      end if;

      case Unit is
         when Weeks =>
            return Image (Item.N) & (if Write_Unit then "w" else "");

         when Months =>

            declare
               Months : constant Scalar_Base := Item.N / 4;
               Weeks  : constant Scalar_Base := Item.N mod 4;
            begin
               if Write_Unit then
                  if Weeks = 0 then
                     return Chop (Natural'Image (Months)) & "m";
                  else
                     return
                       Image (Months) & "m"
                       & Image (Weeks) & "w";
                  end if;
               else
                  return Image (Months);
               end if;
            end;

      end case;

   end Image;
end EU_Projects.Times;
