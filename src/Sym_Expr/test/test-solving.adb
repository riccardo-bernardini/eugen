with Test_Report;                           use Test_Report;
with Symbolic_Expressions.Solving;
with Ada.Strings.Unbounded;

procedure Test.Solving is
   use Ada.Strings.Unbounded;

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   function "+" (X : Unbounded_String) return String
                 renames To_String;


   type Equation_Type is
      record
         Var  : Unbounded_String;
         Expr : Unbounded_String;
      end record;
   --  Type representing an equation <var> = <expr>

   type System is
      array (Positive range <>) of Equation_Type;
   --  System of many equations

   type System_Access is access System;

   type Var_Value_Pair is
      record
         Var   : Unbounded_String;
         Value : Integer;
      end record;

   type Solution is
      array (Positive range <>) of Var_Value_Pair;

   type Solution_Access is access Solution;

   type Test_Case is
      record
         Equations : System_Access;
         Result    : Solution_Access;
         -- Set Result to null if the system is supposed to be not solvable
      end record;

   type Test_Case_Array is
      array (Positive range <>) of Test_Case;

   Test_Cases : constant Test_Case_Array :=
                  ((Equations => new System'
                       ((+"y", +"x*u-3"),
                        (+"u", +"42"),
                        (+"x", +"u/2")),
                    Result    => new Solution'
                       ((+"u", 42),
                        (+"x", 21),
                        (+"y", 879))),
                   (Equations => new System'
                       ((+"y", +"x*u-3"),
                        (+"u", +"42")),
                    Result    => null));

   package Solving is new Int_Expr.Solving;

   function Check (This : Test_Case) return Boolean is
      use Int_Expr.Variable_Tables;
      use Int_Expr;

      Equations : Solving.Equation_Tables.Map;
      Result    : Int_Expr.Variable_Tables.Map;
      Success   : Boolean;
      Pos : Int_Expr.Variable_Tables.Cursor;
      Expected :  Int_Expr.Variable_Tables.Map;
   begin
      for I in This.Equations'Range loop
         Equations.Insert (Key      => +This.Equations (I).Var,
                           New_Item => Parse (+This.Equations (I).Expr));
      end loop;

      if This.Result /= null then
         for I in This.Result'Range loop
            Expected.Insert (Key      => +This.Result (I).Var,
                             New_Item => This.Result (I).Value);
         end loop;
      end if;

      Solving.Triangular_Solve (What    => Equations,
                                Result  => Result,
                                Success => Success);

      if not Success then
         --  Return true if and only if we expected that the system was not
         --  solvable
         return This.Result = null;
      end if;

      if This.Result = null then
         --  Here Success=True, but we expected a non-solvable system
         return False;
      end if;

      pragma Assert (Success and This.Result /= null);

      Pos := Expected.First;
      while Pos /= No_Element loop
         if not Result.Contains (Key (Pos)) then
            return False;
         end if;

         if Result.Element (Key (Pos)) /= Element (Pos) then
            return False;
         end if;

         Next (Pos);
      end loop;

      Pos := Result.First;
      while Pos /= No_Element loop
         if not Expected.Contains (Key (Pos)) then
            return False;
         end if;

         Next (Pos);
      end loop;

      return True;
   end Check;

   procedure Do_Solving_Checks is
      new Do_Suite (Test_Case       => Test_Case,
                    Test_Case_Array => Test_Case_Array,
                    Check           => Check);

   Reporter : Reporter_Type;
begin
   Do_Solving_Checks (Reporter, Test_Cases);

   Final (Reporter);
end Test.Solving;
