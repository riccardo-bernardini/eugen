pragma Ada_2012;
with EU_Projects.Nodes;       use EU_Projects.Nodes;
--  with Ada.Text_IO; use Ada.Text_IO;
with EU_Projects.Times.Time_Expressions.Parsing;
with Ada.Text_IO; use Ada.Text_IO;

package body EU_Projects.Projects.Housekeeping is


   function Var_Of (Pos : All_Node_Cursor;
                    Var : Simple_Identifier)
                    return Dotted_Identifier
   is (Join (Dotted_Identifier (Element (Pos).Label), Var));

   -----------------------------------
   -- Link_Milestone_To_Deliverable --
   -----------------------------------

   procedure Link_Milestone_To_Deliverable (Project : in out Project_Descriptor)
   is
      N              : Node_Access;
      Errors_Occured : Boolean := False;
   begin
      for Pos in Project.All_Deliverables loop
         for Lab of Element (Pos).Linked_Milestones loop
            N := Project.Find (Lab);

            if N = null then
               Put_Line (Standard_Error, "Milestone '" & To_String (Lab) & "' undefined");
               Errors_Occured := True;
            else
               Milestone_Access (N).Add_Deliverable (Element (Pos).Label);
            end if;
         end loop;
      end loop;

      if Errors_Occured then
         raise Housekeeping_Failed;
      end if;
   end Link_Milestone_To_Deliverable;

   ----------------------
   -- Check_Node_Table --
   ----------------------

   procedure Check_Node_Table (Project : in out Project_Descriptor) is
      N : Node_Access;
      Errors_Occured : Boolean := False;
   begin
      for Pos in Project.All_Nodes loop
         N := Element (Pos);

         if not Project.Node_Directory.Contains (N.Label) then
            Put_Line (Standard_Error, "Node with label '" & To_String (N.Label) & "' not in directory");
            Errors_Occured := True;
         end if;
      end loop;


      if Errors_Occured then
         raise Housekeeping_Failed;
      end if;
   end Check_Node_Table;

   -----------------------
   -- Collect_Equations --
   -----------------------

   function Collect_Equations
     (Project : Project_Descriptor)
      return Times.Time_Expressions.Solving.Time_Equation_System
   is
      use Times.Time_Expressions.Solving;
      use Times.Time_Expressions.Parsing;

      Result  : Time_Equation_System;
      Symbols : Symbol_Table;

   begin
      Fill_With_Defaults (Symbols);

      for N in Project.All_Nodes (True) loop
         declare
            Vars : constant Nodes.Variable_List := Element (N).Variables;
         begin
            for Var_Name of Vars loop
               Define_Variable (Symbols, Var_Of (N, Var_Name));
            end loop;
         end;
      end loop;

      for N in Project.All_Nodes (True) loop
         Element (N).Parse_Raw_Expressions (Symbols);

         declare
            Vars : constant Nodes.Variable_List := Element (N).Variables;
         begin
            for Var_Name of Vars loop
               if Element (N).Is_A (Var_Name, Times.Instant_Value) then
                  Add_Equation (Equations => Result,
                                Left      => Var_Of (N, Var_Name),
                                Right     => Element (N).Get_Symbolic_Instant (Var_Name));

               else
                  Add_Equation (Equations => Result,
                                Left      => Var_Of (N, Var_Name),
                                Right     => Element (N).Get_Symbolic_Duration (Var_Name));
               end if;

            end loop;
         end;
      end loop;

      return Result;
   end Collect_Equations;

   --------------------
   -- Assign_Results --
   --------------------

   procedure Assign_Results
     (Project : in out Project_Descriptor;
      Values  : Times.Time_Expressions.Solving.Variable_Map)
   is

   begin
      for N in Project.All_Nodes loop
         declare
            Vars : constant Variable_List := Element (N).Variables;
         begin
            for Var_Name of Vars loop
               if Element (N).Is_A (Var_Name, Times.Instant_Value) then
                  Element (N).Fix_Instant (Var   => Var_Name,
                                           Value => Values (Var_Of (N, Var_Name)));
               end if;
            end loop;
         end;
      end loop;
   end Assign_Results;

end EU_Projects.Projects.Housekeeping;




--        --------------
--        -- Set_Time --
--        --------------
--
--        procedure Set_Event_Time (Node : in out Timed_Node'Class)
--        is
--           use type Times.Instant;
--        begin
--           if not Contains (Values, Node.Due_Time_Var) then
--              raise Constraint_Error;
--           end if;
--
--           declare
--              T : constant Times.Instant := Get (Values, Node.Due_Time_Var);
--           begin
--              if not Node.Time_Fixed then
--                 Node.Due_On (T);
--
--              elsif T = Node.Due_On then
--                 null;
--
--              else
--                 raise Constraint_Error;
--              end if;
--           end;
--        end Set_Event_Time;
--
--        --------------
--        -- Set_Time --
--        --------------
--
--        procedure Set_Action_Time (Node : in out Action_Node'Class)
--        is
--           use type Times.Instant;
--        begin
--           for Event in Action_Nodes.Event_Type loop
--              if not Contains (Values, Node.Event_Var (Event)) then
--                 raise Constraint_Error;
--              end if;
--
--              declare
--                 T : constant Times.Instant := Get (Values, Node.Event_Var (Event));
--              begin
--
--                 if not Node.Time_Fixed (Event) then
--                    Node.Set_Time (Event => Event, Time  => T);
--
--                 elsif T = Node.Event_Time (Event)  then
--                    null;
--
--                 else
--                    raise Constraint_Error;
--                 end if;
--              end;
--           end loop;
--        end Set_Action_Time;
--
--        for Tsk in Project.All_Tasks loop
--           Set_Action_Time (Element (Tsk).all);
--        end loop;
--
--        for Deliv in Project.All_Deliverables loop
--           Set_Event_Time (Element (Deliv).all);
--        end loop;
--
--        for Milestone in Project.All_Milestones loop
--           Set_Event_Time (Element (Milestone).all);
--        end loop;
-------------------------------------------------------------------
--           Add_Equation (Equations => Result,
--                         Left      => Element (WP).Ending_Time_Var,
--                         Right     => Element (WP).Ending_Time);
--
--           Add_Equation (Equations => Result,
--                         Left      => Element (WP).Starting_Time_var,
--                         Right     => Element (WP).Starting_Time);
--        end loop;

--        for Tsk in Project.All_Tasks loop
--           Add_Equation (Equations => Result,
--                         Left      => Element (Tsk).Ending_Time_Var,
--                         Right     => Element (Tsk).Ending_Time);
--
--           Add_Equation (Equations => Result,
--                         Left      => Element (Tsk).Starting_Time_Var,
--                         Right     => Element (Tsk).Starting_Time);
--        end loop;
--
--        for Deliv in Project.All_Deliverables loop
--           Add_Equation (Equations => Result,
--                         Left      => Element(Deliv).Due_Time_Var,
--                         Right     => Element(Deliv).Due_On);
--        end loop;
--
--        for Milestone in Project.All_Milestones loop
--           Add_Equation (Equations => Result,
--                         Left      => Element(Milestone).Due_Time_Var,
--                         Right     => Element(Milestone).Due_On);
--        end loop;
---------------------------------------------------------------------
