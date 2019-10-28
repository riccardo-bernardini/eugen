pragma Ada_2012;
package body EU_Projects.Times.Time_Expressions.Solving is



  ------------------
   -- Add_Equation --
   ------------------

   procedure Add_Equation (Equations : in out Time_Equation_System;
                           Left      : Dotted_Identifier;
                           Right     : Symbolic_Duration)
   is
   begin
      if Contains_Left_Term (Equations, Left) then
         raise Constraint_Error;
      end if;

      Equations.M.Insert (Key      => Left,
                          New_Item => (Duration_Value, Right.D));
   end Add_Equation;

   ------------------
   -- Add_Equation --
   ------------------

   procedure Add_Equation (Equations : in out Time_Equation_System;
                           Left      : Dotted_Identifier;
                           Right     : Symbolic_Instant)
   is
   begin
      if Contains_Left_Term (Equations, Left) then
         raise Constraint_Error;
      end if;
--        Put_Line ("ADD(" & To_String (Left) & "," & Time_Expr.Dump (Right.T) & ")");

      Equations.M.Insert (Key      => Left,
                          New_Item => (Instant_value, Right.T));
   end Add_Equation;

   -----------
   -- Solve --
   -----------

   function Solve (Equations : Time_Equation_System) return Variable_Map
   is

      Eq : Time_Equations.Equation_Tables.Map;
      R  : Time_Expr.Variable_Tables.Map;
      Success : Boolean;
   begin
      for C in Equations.M.Iterate loop
         Eq.Insert (Key      => Equation_Maps.Key (C),
                    New_Item => Equation_Maps.Element (C).Val);
      end loop;

      Time_Equations.Triangular_Solve (What    => Eq,
                                       Result  => R,
                                       Success => Success);

      if not Success then
         raise Unsolvable;
      end if;

      declare
         Result : Variable_Map;
      begin
         for C in R.Iterate loop
            declare
               use Time_Expr.Variable_Tables;

               ID : constant Dotted_Identifier := Key (C);
               Val : constant Scalar_Type := Element (C);
            begin
               case Equations.M.Element (ID).Class is
                  when Instant_Value =>
                     Result.M.Insert (Key      => ID,
                                      New_Item => (Class => Instant_Value,
                                                   I     => Instant (Val)));

                  when Duration_Value =>
                     Result.M.Insert (Key      => ID,
                                      New_Item => (Class => Duration_Value,
                                                   D     => Duration (Val)));
               end case;
            end;
         end loop;

         return Result;
      end;
   end Solve;

end EU_Projects.Times.Time_Expressions.Solving;
