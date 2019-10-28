generic
package Symbolic_Expressions.Solving is
   -- ==================== --
   -- == SYSTEM SOLVING == --
   -- ==================== --

    package Equation_Tables is
      new Ada.Containers.Indefinite_Ordered_Maps
         (Key_Type     => Variable_Name,
          Element_Type => Symbolic_Expression);

   procedure Triangular_Solve (What    : in     Equation_Tables.Map;
                               Result  :    out Variable_Tables.Map;
                               Success :    out Boolean);
   --  Parameter What represents a system of equations of the form
   --
   --           <variable> = <expression>
   --
   --  This procedure tries to find a solution for that system of
   --  equations.  If a solution is found, Success is set to True and Result
   --  contains the values found for the variables; if a solution is not
   --  found Success is set to False and Result can be partially filled.
   --
   --  To be honest, this procedure is limited to solving
   --  systems that can be solved by "back substitution," such as
   --
   --            x = y*3 + max(u, v)
   --            y = min(u, z)
   --            u = v*z
   --            v = 10
   --            z = -1
   --
   --  Note that the system above can be solved by evaluating
   --  the expressions from last to first.  Systems like
   --
   --            x + y = 0
   --            x - y = 4
   --
   --  have a solution, but they cannot be solved by this procedure.
   --  This procedure is useful to solve for constraints that come, for
   --  example, from a DAG and it has the advantage that no special constraints
   --  are posed on the equations that can include non-linear (and badly
   --  behaving) functions like max, abs, and so on...
   --
end Symbolic_Expressions.Solving;
