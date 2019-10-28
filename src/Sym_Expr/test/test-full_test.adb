----------------------------------------------------------------------------
--            Symbolic Expressions (symexpr)
--
--               Copyright (C) 2012, Riccardo Bernardini
--
--      This file is part of symexpr.
--
--      symexpr is free software: you can redistribute it and/or modify
--      it under the terms of the Lesser GNU General Public License as published by
--      the Free Software Foundation, either version 3 of the License, or
--      (at your option) any later version.
--
--      symexpr is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the Lesser GNU General Public License
--      along with gclp.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Test_Report;             use Test_Report;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);


procedure Test.Full_Test is
   use Int_Expr;

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   function "+" (X : Unbounded_String) return String
                 renames To_String;

   type Var_Descriptor is
      record
         Name  : Unbounded_String;
         Value : Integer;
      end record;

   type Var_Array is
      array (Positive range <>) of Var_Descriptor;

   Variables : constant Var_Array :=
                 ((+"foo", 12),
                  (+"bar", -3),
                  (+"with_under", 1),
                  (+"with.dot", -3),
                  (+"with__two", 4));


   type Test_Case is
      record
         Expr            : Unbounded_String;  -- To be parsed
         Syntax_OK       : Boolean;           -- Is Expr syntax correct?
         Free_Var        : Boolean;           -- Has Expr free variables?
         Free_After_Subs : Boolean;           -- Will Expr have free var
         Value           : Integer;
      end record;

   type Test_Case_Array is
      array (Positive range <>) of Test_Case;

   Test_Cases : constant Test_Case_Array :=
                  ((Expr            => +"3+4*-5",
                    Syntax_OK       => True,
                    Free_Var        => False,
                    Free_After_Subs => False,
                    Value           => -17),
                   (Expr            => +"4*(3+5)*-bar",
                    Syntax_OK       => True,
                    Free_Var        => True,
                    Free_After_Subs => False,
                    Value           => 96),
                   (Expr            => +"(-4)/(3+-5)/pippo",
                    Syntax_OK       => True,
                    Free_Var        => True,
                    Free_After_Subs => True,
                    Value           => 0),
                   (Expr            => +"(-4)//(3+-5)/pippo",
                    Syntax_OK       => False,
                    Free_Var        => True,
                    Free_After_Subs => True,
                    Value           => 0),
                   (Expr            => +"(-4)/(3+-5/pippo",
                    Syntax_OK       => False,
                    Free_Var        => True,
                    Free_After_Subs => True,
                    Value           => 0),
                   (Expr            => +" bar  * foo + max (bar,foo) ",
                    Syntax_OK       => True,
                    Free_Var        => True,
                    Free_After_Subs => False,
                    Value           => -24),
                   (Expr            => +"     ",
                    Syntax_OK       => False,
                    Free_Var        => True,
                    Free_After_Subs => False,
                    Value           => -24),
                   (Expr            => +" -(+bar - foo + magic ()) ",
                    Syntax_OK       => True,
                    Free_Var        => True,
                    Free_After_Subs => False,
                    Value           => -27),
                   (Expr            => +" -(+bar - foo + max (3, 4**6, 12)) ",
                    Syntax_OK       => False,
                    Free_Var        => True,
                    Free_After_Subs => False,
                    Value           => -27)
                  );

   Var_Table  : Variable_Tables.Map;

   function Check (This : Test_Case) return Boolean is
      Ori      : Symbolic_Expression;
      Subs     : Symbolic_Expression;
      Raised   : Boolean;
      Expected : Boolean;
   begin
      Raised := False;
      Expected := False;

      begin
         Ori := Parse (+This.Expr);
      exception
         when Parsing_Error =>
            Raised := True;
            Expected := True;
         when others =>
            Raised := True;
            Expected := False;
      end;

      if Raised then
         if not Expected then
            return False;
         else
            return not This.Syntax_OK;
         end if;
      end if;

      if (not This.Free_Var) /= Is_Constant (Ori) then
         return False;
      end if;

      Subs := Replace (Ori, Var_Table);
      if (not This.Free_After_Subs) /= Is_Constant (Subs) then
         return False;
      end if;

      if not This.Free_After_Subs then
         return Eval (Subs) = This.Value;
      else
         Raised := False;

         declare
            Junk : Integer;
            pragma Unreferenced (Junk);
         begin
            Junk := Eval (Subs);
         exception
            when Not_A_Scalar =>
               Raised := True;
         end;

         return Raised;
      end if;

   end Check;

   procedure Do_Checks is
      new Do_Suite (Test_Case       => Test_Case,
                    Test_Case_Array => Test_Case_Array,
                    Check           => Check);

   type Syntax_ID_Case is
      record
         Expr    : Unbounded_String;
         Action  : Unknown_ID_Action_Type;
         Success : Boolean;
      end record;

   type Syntax_ID_Case_Array is
      array (Positive range <>) of Syntax_ID_Case;

   Syntax_Cases : constant Syntax_ID_Case_Array :=
                    ((Expr       => +"4*foo",
                      Action      => Die,
                      Success     => False),
                     (Expr        => +"4*foo",
                      Action      => OK,
                      Success     => True),
                     (Expr        => +"4*foo()",
                      Action      => Accept_As_Var,
                      Success     => False),
                     (Expr        => +"4*bar(2,3) + pippo",
                      Action      => Die,
                      Success     => True),
                     (Expr        => +"4*pippo(2,3)",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +"4*bar",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +"4*bar(2)",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +"4*bar(2,3,4)",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +"4*pluto(2)",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +"4*pluto(2,3)",
                      Action      => OK,
                      Success     => True),
                     (Expr        => +"4*pluto(2,3,4)",
                      Action      => OK,
                      Success     => True),
                     (Expr        => +"zorro()",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +"zorro(1)",
                      Action      => OK,
                      Success     => False),
                     (Expr        => +" zorro (1,  2)",
                      Action      => OK,
                      Success     => True),
                     (Expr        => +"zorro(1, 2, 3)",
                      Action      => OK,
                      Success     => True),
                     (Expr        => +"zorro(1, 2, 3,4)",
                      Action      => OK,
                      Success     => True),
                     (Expr        => +"zorro(1, 2, 3,4,x)",
                      Action      => OK,
                      Success     => False));





   ID_Table : ID_Table_Type;

   function Check_Syntax (This : Syntax_ID_Case) return Boolean is
      Tmp      : Symbolic_Expression;
      pragma Unreferenced (Tmp);
      Raised   : Boolean;
      Expected : Boolean;
   begin
      Raised := False;
      Expected := False;

      begin
         Tmp := Parse (Input         => +This.Expr,
                       ID_Table      => ID_Table,
                       On_Unknown_ID => This.Action);
      exception
         when Parsing_Error =>
            Raised := True;
            Expected := True;
         when others =>
            Raised := True;
            Expected := False;
      end;

      if Raised then
         if not Expected then
            return False;
         else
            return not This.Success;
         end if;
      else
         return This.Success;
      end if;
   end Check_Syntax;

   procedure Do_Syntax_ID_Checks is
      new Do_Suite (Test_Case       => Syntax_ID_Case,
                    Test_Case_Array => Syntax_ID_Case_Array,
                    Check           => Check_Syntax);



   Reporter : Reporter_Type;
begin
   --     Be_Verbose (Reporter);
   Define_Function (ID_Table, "bar", Exactly (2));
   Define_Function (ID_Table, "pluto", At_Least (2));
   Define_Function (ID_Table, "zorro", Between (2, 4));
   Define_Variable (ID_Table, "pippo");

   for I in Variables'Range loop
      Var_Table.Insert (+Variables (I).Name, Variables (I).Value);
   end loop;

   Do_Checks (Reporter, Test_Cases, "Basic");

   Do_Syntax_ID_Checks (Reporter, Syntax_Cases, "Syntax");

   declare
      X : constant Symbolic_Expression := Variable ("x");
      Y : constant Symbolic_Expression := Variable ("y");
      T : constant Symbolic_Expression := (X + Y) * (X - Y);
      U : constant Symbolic_Expression := Function_Call ("max", (X, Y));
      R : constant Symbolic_Expression := Function_Call ("min", (X, Y));
      S : constant Symbolic_Expression := R * U;
      K : constant Symbolic_Expression := To_Expr (42);
      M : constant Symbolic_Expression := X + Y * U / (K - S);
      Q : constant Symbolic_Expression := Replace (S, "x", T);
      Z : constant Symbolic_Expression := T * (- Q);
      W : constant Symbolic_Expression := +T * (+T);
      A : constant Symbolic_Expression := W * 3-(2 + U) / (Q - 1);

      Val_X : constant Integer := 12;
      Val_Y : constant Integer := -3;
      Val_T : constant Integer := (Val_X + Val_Y) * (Val_X - Val_Y);
      Val_U : constant Integer := Integer'Max (Val_X, Val_Y);
      Val_R : constant Integer := Integer'Min (Val_X, Val_Y);
      Val_S : constant Integer := Val_R * Val_U;
      Val_K : constant Integer := 42;
      Val_M : constant Integer := Val_X + Val_Y * Val_U / (Val_K - Val_S);
      Val_Q : constant Integer :=
                Integer'Max (Val_Y, Val_T) * Integer'Min (Val_Y, Val_T);
      Val_Z : constant Integer := Val_T * (-Val_Q);
      Val_W : constant Integer := +Val_T * (+Val_T);
      Val_A : constant Integer := Val_W * 3-(2 + Val_U) / (Val_Q - 1);

      function Check (Item     : Symbolic_Expression;
                      Expected : Integer)
                      return Boolean
      is
         Tmp : Symbolic_Expression;
      begin
         Tmp := Replace (Item, "x", Val_X);
         Tmp := Replace (Tmp, "y", Val_Y);

         return Is_Constant (Tmp) and then Eval (Tmp) = Expected;
      end Check;
   begin
      New_Suite (Reporter, "Operators");
      New_Result (Reporter, Check (X, Val_X));
      New_Result (Reporter, Check (Y, Val_Y));
      New_Result (Reporter, Check (T, Val_T));
      New_Result (Reporter, Check (U, Val_U));
      New_Result (Reporter, Check (R, Val_R));
      New_Result (Reporter, Check (S, Val_S));
      New_Result (Reporter, Check (K, Val_K));
      New_Result (Reporter, Check (M, Val_M));
      New_Result (Reporter, Check (Q, Val_Q));
      New_Result (Reporter, Check (Z, Val_Z));
      New_Result (Reporter, Check (W, Val_W));
      New_Result (Reporter, Check (A, Val_A));
   end;

   Final (Reporter);
end Test.Full_Test;
