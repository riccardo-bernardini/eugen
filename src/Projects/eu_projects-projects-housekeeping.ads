with EU_Projects.Times.Time_Expressions.Solving;

private package EU_Projects.Projects.Housekeeping is
   procedure Link_Milestone_To_Deliverable (Project : in out Project_Descriptor);

   procedure Check_Node_Table (Project : in out Project_Descriptor);

   function Collect_Equations (Project : Project_Descriptor)
                               return Times.Time_Expressions.Solving.Time_Equation_System;

   procedure Assign_Results (Project : in out Project_Descriptor;
                             Values  : Times.Time_Expressions.Solving.Variable_Map);

   Housekeeping_Failed : exception;
end EU_Projects.Projects.Housekeeping;
