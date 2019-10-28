--  with Ada.Text_IO; use Ada.Text_IO;

package body EU_Projects.Times.Time_Expressions is

   -------------
   -- Extract --
   -------------

   function Extract (X : Symbolic_Instant_Array) return Time_Expr.Expression_Array
   is
      Result : Time_Expr.Expression_Array (X'Range);
   begin
      for I in X'Range loop
         Result (I) := X (I).T;
      end loop;

      return Result;
   end Extract;

   function Min (X : Symbolic_Instant_Array) return Symbolic_Instant
   is ((T => Time_Expr.Function_Call (Min_Function, Extract (X))));

   function Max (X : Symbolic_Instant_Array) return Symbolic_Instant
   is  ((T => Time_Expr.Function_Call (Max_Function, Extract (X))));


   ----------
   -- Call --
   ----------

   function Call (Name  : Dotted_Identifier;
                  Param : Scalar_Array)
                  return Scalar_Type
   is
      Result : scalar_Type;
   begin
      if Name = Min_Function then
         Result := Param (Param'First);

         for I in Param'Range loop
            Result := Min (Result, Param (I));
         end loop;

      elsif Name = "max" then
         Result := Param (Param'First);

         for I in Param'Range loop
            Result := Max (Result, Param (I));
         end loop;

      elsif Name = "lt" then
         Result := (if Instant(Param (Param'First)) < Instant(Param (Param'First + 1))
                    then True
                    else Untrue);

      elsif Name = "le" then
         Result := (if Instant(Param (Param'First)) <= Instant(Param (Param'First + 1))
                    then True
                    else Untrue);

      elsif Name = "gt" then
         Result := (if Instant(Param (Param'First)) > Instant(Param (Param'First + 1))
                    then True
                    else Untrue);

      elsif Name = "ge" then
         Result := (if Instant(Param (Param'First)) >= Instant(Param (Param'First + 1))
                    then True
                    else Untrue);


      elsif Name = "eq" then
         Result := (if Param (Param'First) = Param (Param'First + 1)
                    then True
                    else Untrue);

      elsif Name = "ne" then
         Result := (if Param (Param'First) /= Param (Param'First + 1)
                    then True
                    else Untrue);

      elsif Name = "not" then
         Result := (if Param (Param'First) = Untrue
                    then True
                    else Untrue);

      elsif Name = "if" then
         Result := (if Param (Param'First) = True
                    then Param (Param'First + 1)
                    else Param (Param'First + 2));
      else
         raise Constraint_Error;
      end if;

      return Result;
   end Call;



--     function Image (Item : Scalar_Type) return String
--     is
--     begin
--        return Scalar_Type'Image (Item);
--     end Image;




end EU_Projects.Times.Time_Expressions;
