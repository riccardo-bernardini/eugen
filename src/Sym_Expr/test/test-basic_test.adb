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

with Ada.Text_Io;

procedure Test.Basic_Test is

   use Int_Expr;


--     function To_S (X : Integer) return String is
--     begin
--        return Integer'Image (X);
--     end To_S;

   X : Symbolic_Expression;-- := Parse ("3+4*(5+1)");
   Z : Symbolic_Expression;-- := Parse ("3+4*(5+1-min(3, 4*2))");
   Y : constant Symbolic_Expression := Parse ("3+4*(5+1-max  ( pluto.end-3, pippo.end*2))");
   U : constant Symbolic_Expression := Variable ("u");
   T : constant Symbolic_Expression := To_Expr (4) - To_Expr (2) * U + U * U;
   Q : Symbolic_Expression;
   R : Symbolic_Expression;

   Tbl : Variable_Tables.Map;
begin
   Tbl.Include ("pippo.end", -6);
   Tbl.Include ("pluto.end", 7);

   Ada.Text_Io.Put_Line (Dump (Y));
   Ada.Text_Io.Put_Line (Dump (T));
   Ada.Text_Io.Put_Line (Dump (T*Y));

   X := Replace (Item     => Y,
                 Var_Name => "pluto.end",
                 Value    => 7);
   Ada.Text_Io.Put_Line (Dump (X));

   Z := Replace (Item     => X,
                 Var_Name => "pippo.end",
                 Value    => -6);
   Ada.Text_Io.Put_Line (Dump (Z));

   Q := Replace (Y, Tbl);

   R := Replace (T, "u", Y);

   Ada.Text_Io.Put_Line (Dump (R));

   Ada.Text_IO.Put_Line (Integer'Image (Eval (Z)));
   Ada.Text_IO.Put_Line (Integer'Image (Eval (Q)));
   Ada.Text_IO.Put_Line (Integer'Image (Eval (Replace (T, "u", 5))));

   Ada.Text_IO.Put_Line (Boolean'Image (Is_Constant (Q)));
   Ada.Text_IO.Put_Line (Boolean'Image (not Is_Constant (X)));
   Ada.Text_IO.Put_Line (Boolean'Image (not Is_Constant (Y)));
   Ada.Text_IO.Put_Line (Boolean'Image (not Free_Variables (X).Is_Empty));
   Ada.Text_IO.Put_Line (Boolean'Image (not Free_Variables (T).Is_Empty));
   Ada.Text_IO.Put_Line (Boolean'Image (Free_Variables (Q).Is_Empty));
end Test.Basic_Test;
