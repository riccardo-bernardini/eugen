with Ada.Text_IO; use Ada.Text_IO;
package body Symbolic_Expressions.Solving is
   procedure Mastica (X : Boolean) is
   begin
      null;
   end Mastica;
   ----------------------
   -- Triangular_Solve --
   ----------------------

   procedure Triangular_Solve
     (What    : in     Equation_Tables.Map;
      Result  :    out Variable_Tables.Map;
      Success :    out Boolean)
   is
      use Equation_Tables;

      Verbose : constant Boolean := True;
      Copy : Equation_Tables.Map := What;
      Buffer : Equation_Tables.Map;
      Something_Done : Boolean;
      Expr : Symbolic_Expression;
      Pos : Cursor;
   begin
--        for Pos in What.Iterate loop
--           Put_Line (">> " & id_image (Key (Pos)) & "=" & Dump (Element (Pos)));
--        end loop;
      --
      --  The algorithm used is very simple and general, although maybe
      --  not the most efficient (since its cost can grow as N*N).  We
      --  iterate over the set of equations, replacing variables according
      --  to the partial solution stored in Result.  If, as a consequence
      --  of such substitution, an expression becomes constant, it is
      --  removed from the set of equations and stored in Result.  The
      --  iteration will end when the system becomes empty or when
      --  no equation are moved from the system to the solution
      --
      Something_Done := True;

      while (not Copy.Is_Empty) and Something_Done loop
         Buffer.Clear;
         Something_Done := False;

         --  Mastica does nothing, but without this Something_Done
         --  seems to remain stuck to True.  Optimizer bug?
         Mastica (Something_Done);

         Pos := Copy.First;
         while Pos /= No_Element loop
            Expr := Replace (Element (Pos), Result);

            if Is_Constant (Expr) then
               Result.Insert (Key (Pos), Eval (Expr));
               Something_Done := True;
            else
               Buffer.Insert (Key(Pos), Expr);
            end if;

            Next (Pos);
         end loop;

         Copy := Buffer;
      end loop;

      Success := Copy.Is_Empty;

      if not Success and Verbose then
         for Pos in Copy.Iterate loop
            Put_Line ("-->" & ID_image (Key (Pos)) & "=" & Dump (Element (Pos)));
         end loop;
      end if;
   end Triangular_Solve;

end Symbolic_Expressions.Solving;
