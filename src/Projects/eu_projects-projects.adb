with EU_Projects.Times.Time_Expressions.Solving;
with EU_Projects.Projects.Housekeeping;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body EU_Projects.Projects is

   -----------
   -- Bless --
   -----------

   procedure Bless (Project : in out Project_Descriptor;
                    Info    : Nodes.Timed_Nodes.Project_Infos.Info_Access)
   is
   begin
      Project.Info := Info;
   end Bless;

   ---------------------
   -- Milestone_Table --
   ---------------------

   function Milestone_Table (Item : Project_Descriptor)
                             return Milestone_Table_Type
   is
      use Times;


      function Due_On (X : Milestone_Cursor) return Extended_Month_Number
      is ((if Element (X).Due_On = To_Be_Decided
           then
              No_Month
           else
              Month_Number (Months (Element (X).Due_On))));

      type Counter_Array is array (Month_Number range <>) of Natural;

      Last_Month : constant Month_Number :=
                     (if Item.Last_Month = To_Be_Decided
                      then
                         36
                      else
                         Month_Number (Months (Item.Last_Month)));

      Counters    : Counter_Array (1 .. Last_Month) := (others => 0);
      Max_Counter : Natural := 0;
   begin
      for Mstone in Item.All_Milestones loop
         declare
            Due : constant Extended_Month_Number := Due_On (Mstone);
         begin
            if Due /= No_Month then
               Counters (Due) := Counters (Due)+1;

               Max_Counter := Natural'Max (Max_Counter, Counters (Due));
            end if;
         end;
      end loop;

      declare
         Result : Milestone_Table_Type (Counters'Range, 1 .. Max_Counter) :=
                    (others => (others => null));
      begin
         Counters := (others => 0);

         for Mstone in Item.All_Milestones loop
            declare
               Due : constant Extended_Month_Number := Due_On (Mstone);
            begin
               if Due /= No_Month then
                  Counters (Due) := Counters (Due)+1;

                  Result (Due, Counters (Due)) := Element (Mstone);
               end if;
            end;
         end loop;

         return Result;
      end;
   end Milestone_Table;

   -------------------
   -- Partner_Names --
   -------------------

   function Partner_Names (Item : Project_Descriptor) return Partners.Partner_Name_Array
   is
      First  : constant Partners.Partner_Index := Item.Partner_List.First_Index;
      Last   : constant Partners.Partner_Index := Item.Partner_List.Last_Index;
      Result : Partners.Partner_Name_Array (First .. Last);
   begin
      for Idx in Result'Range loop
         Result (Idx) := Partners.Partner_Label (Item.Partner_List (Idx).all.Label);
      end loop;

      return Result;
   end Partner_Names;

   -----------
   -- First --
   -----------

   function First (Object : All_Task_Iterator) return All_Task_Cursor
   is
      Result : All_Task_Cursor;
   begin
      Result.WP_Pos := First (Object.WP_Iter);


      loop
         exit when not Has_Element (Result.WP_Pos);

         Result.Task_Iter := WPs.Task_Iterator (Element (Result.WP_Pos).All_Tasks);
         Result.Task_Pos  := First (Result.Task_Iter);

         exit when Has_Element (Result.Task_Pos);

         Result.WP_Pos := Next (Object.WP_Iter, Result.WP_Pos);
      end loop;

      return Result;
   end First;

   -----------
   -- First --
   -----------

   function First (Object : All_Deliverable_Iterator) return All_Deliverable_Cursor
   is
      Result : All_Deliverable_Cursor;
   begin
      Result.WP_Pos := First (Object.WP_Iter);


      loop
         exit when not Has_Element (Result.WP_Pos);

         Result.Deliv_Iter := WPs.Deliverable_Iterator (Element (Result.WP_Pos).All_Deliverables);
         Result.Deliv_Pos  := Result.Deliv_Iter.First;

         exit when Has_Element (Result.Deliv_Pos);

         Result.WP_Pos := Next (Object.WP_Iter, Result.WP_Pos);
      end loop;

      return Result;
   end First;


   ----------
   -- Next --
   ----------

   function Next
         (Object   : All_Task_Iterator;
          Position : All_Task_Cursor) return All_Task_Cursor
   is
      Result : All_Task_Cursor := Position;
   begin
      Result.Task_Pos := Next (Result.Task_Iter, Result.Task_Pos);

      loop
         exit when Has_Element (Result.Task_Pos);

         Result.WP_Pos := Next (Object.WP_Iter, Result.WP_Pos);

         exit when not Has_Element (Result.Wp_Pos);

         Result.Task_Iter := WPs.Task_Iterator (Element (Result.WP_Pos).All_Tasks);
         Result.Task_Pos  := First (Result.Task_Iter);
      end loop;

      return Result;
   end Next;

   ----------
   -- Next --
   ----------

   function Next
         (Object   : All_Deliverable_Iterator;
          Position : All_Deliverable_Cursor) return All_Deliverable_Cursor
   is
      Result : All_Deliverable_Cursor := Position;
   begin
      Result.Deliv_Pos := Wps.Next (Result.Deliv_Iter, Result.Deliv_Pos);

      loop
         exit when Has_Element (Result.Deliv_Pos);

         Result.WP_Pos := Next (Object.WP_Iter, Result.WP_Pos);

         exit when not Has_Element (Result.Wp_Pos);

         Result.Deliv_Iter := WPs.Deliverable_Iterator (Element (Result.WP_Pos).All_Deliverables);
         Result.Deliv_Pos  := Result.Deliv_Iter.First;
      end loop;

      return Result;
   end Next;


   ----------
   -- Next --
   ----------

   function Next
         (Object   : All_Node_Iterator;
          Position : All_Node_Cursor) return All_Node_Cursor
   is
      Result       : All_Node_Cursor := Position;
      Make_New     : Boolean;
      End_Of_Group : Boolean;
   begin
      if not Has_Element (Position) then
         return Position;
      end if;

      Make_New := False;

      if Position.Prj_Info_Access /= null then

         Result.Prj_Info_Access := null;
         return Result;
      end if;

      loop
         End_Of_Group := False;

         case Result.Current_Class is
            when WP_Node =>
               Result.WP_Pos := (if Make_New then
                                    First (Object.WP_Iter)
                                 else
                                    Next (Object.WP_Iter, Result.WP_Pos));

               End_Of_Group := not Has_Element (Result.WP_Pos);
            when Task_Node =>
               Result.Task_Pos := (if Make_New then
                                      First (Object.All_Task_Iter)
                                   else
                                      Next (Object.All_Task_Iter, Result.Task_Pos));

               End_Of_Group := not Has_Element (Result.Task_Pos);

            when Deliverable_Node =>
               Result.Deliverable_Pos := (if Make_New then
                                             First (Object.Deliverable_Iter)
                                          else
                                             Next (Object.Deliverable_Iter, Result.Deliverable_Pos));

               End_Of_Group := not Has_Element (Result.Deliverable_Pos);

            when Risk_Node =>
               Result.Risk_Pos := (if Make_New then
                                      First (Object.Risk_Iter)
                                   else
                                      Next (Object.Risk_Iter, Result.Risk_Pos));

               End_Of_Group := not Has_Element (Result.Risk_Pos);

            when Partner_Node =>
               Result.Partner_Pos := (if Make_New then
                                         First (Object.Partner_Iter)
                                      else
                                         Next (Object.Partner_Iter, Result.Partner_Pos));

               End_Of_Group := not Has_Element (Result.Partner_Pos);

            when Milestone_Node =>
               Result.Milestone_Pos := (if Make_New then
                                           First (Object.Milestone_Iter)
                                        else
                                           Next (Object.Milestone_Iter, Result.Milestone_Pos));

               End_Of_Group := not Has_Element (Result.Milestone_Pos);
         end case;

         exit when not End_Of_Group;

         exit when Result.Current_Class = Node_Class'Last;

         Result.Current_Class := Node_Class'Succ (Result.Current_Class);
         Make_New := True;

      end loop;

      return Result;
   end Next;

   --     --------------
   --     -- Add_Node --
   --     --------------
   --
   --     procedure Add_Node (Project : in out Project_Descriptor;
   --                         Label   : Nodes.Node_Label;
   --                         Node    : Nodes.Node_Access)
   --     is
   --     begin
   --        Project.Node_Directory.Insert (Key      => Label,
   --                                       New_Item => Node);
   --     end Add_Node;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
         (Project : in out Project_Descriptor;
          Name    : String;
          Value   : String)
   is
   begin
      Project.Options.Insert (Key      => Name,
                              New_Item => Value);
   end Configure;

   -----------------
   -- Add_Partner --
   -----------------

   procedure Add_Partner
         (Project : in out Project_Descriptor;
          Partner : in     Nodes.Partners.Partner_Access)
   is
   begin
      Project.Partner_List.Append (Partner);
      Partner.Set_Index (Project.Partner_List.Last_Index);
   end Add_Partner;

   ------------
   -- Add_WP --
   ------------

   procedure Add_WP
         (Project : in out Project_Descriptor;
          WP      : in     WPs.Project_WP_Access)
   is
   begin
      Project.WP_List.Append (WP);
      WP.Set_Index (Project.WP_List.Last_Index);
   end Add_WP;

   -------------------
   -- Add_Milestone --
   -------------------

   procedure Add_Milestone
         (Project : in out Project_Descriptor;
          Item    : in     Milestones.Milestone_Access)
   is
   begin
      Project.Milestones.Append (Item);
      Item.Set_Index (Project.Milestones.Last_Index);
   end Add_Milestone;

   --------------
   -- Add_Risk --
   --------------

   procedure Add_Risk
         (Project : in out Project_Descriptor;
          Item    : in     Risks.Risk_Access)
   is
   begin
      Project.Risk_List.Append (Item);
   end Add_Risk;




   ----------
   -- Find --
   ----------

   function Find
         (Where : Project_Descriptor;
          Label : Nodes.Node_Label)
          return Nodes.Node_Access
   is
      use Node_Tables;
   begin

      if Where.Node_Directory.Contains (Label) then
         --           Put_Line ("TROVATO");
         return Where.Node_Directory (Label);
      else
         --           Where.Node_Directory.Dump;

         --           Put_Line ("NON TROVATO");
         return null;
      end if;
   end Find;

   ------------
   -- Exists --
   ------------

   function Exists
         (Where : Project_Descriptor;
          Label : Nodes.Node_Label)
          return Boolean
   is (Where.Node_Directory.Contains (Label));

   --------------
   -- Complete --
   --------------

   procedure Freeze (Project : in out Project_Descriptor;
                     Options : Freezing_Options := Sort_Milestones) is
      Equations : Times.Time_Expressions.Solving.Time_Equation_System;
      Results   : Times.Time_Expressions.Solving.Variable_Map;

      procedure Compute_Project_Duration is
         use Times;

         Last_Month : Times.Instant := Times.Earliest_Istant;
         T          : Times.Instant;
      begin
         for WP in Project.All_WPs loop
            T := Element (Wp).Ending_Time;

            if T /= Times.To_Be_Decided then
               Last_Month := (if Last_Month >= T then Last_Month else T);
            end if;
         end loop;

         Project.Last_Month := Last_Month;
      end Compute_Project_Duration;
   begin
      if Project.Freezed then
         return;
      end if;

      Housekeeping.Check_Node_Table (Project);
      Housekeeping.Link_Milestone_To_Deliverable (Project);

      Equations := Housekeeping.Collect_Equations (Project);
      Results := Times.Time_Expressions.Solving.Solve (Equations);

      Housekeeping.Assign_Results (Project, Results);

      Compute_Project_Duration;

      if Options (Milestone_Sorting) then
         Milestone_Sort.Sort (Project.Milestones);

         for pos in Project.Milestones.Iterate loop
            Project.Milestones (Pos).all.Update_Index (Milestone_Vectors.To_Index (Pos));
         end loop;
      end if;

      Project.Freezed := True;
   exception
      when Error: Housekeeping.Housekeeping_Failed =>
         raise Bad_Input
           with  Ada.Exceptions.Exception_Message (Error);
   end Freeze;

end EU_Projects.Projects;

--              declare
--                 procedure Update (Mstone : in out Nodes.Timed_Nodes.Milestones.Milestone_Access)
--                 is
--                 begin
--                    Mstone.Update_Index ();
--                 end Update;
--              begin
--
--                 Project.Milestones.Update_Element (Position   => pos,
--                                                    Process => Update'access);
--              end;


