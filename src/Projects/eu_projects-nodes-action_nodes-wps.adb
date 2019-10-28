with Ada.Finalization; use Ada.Finalization;

package body EU_Projects.Nodes.Action_Nodes.WPs is

   overriding
   function Next
     (Object   : Deliverable_Iterator;
      Position : Deliverable_Cursor) return Deliverable_Cursor
   is
      pragma Unreferenced (Object);
   begin
      if Position.Clone = Deliverable_Vectors.Element (Position.Top_Level).Max_Clone then
         return Deliverable_Cursor'(Top_Level => Deliverable_Vectors.Next(Position.Top_Level),
                                    Clone     => 0);
      else
         return Deliverable_Cursor'(Top_Level => Position.Top_Level,
                                    Clone     => Position.Clone + 1);
      end if;
   end Next;

   function Element (Index : Deliverable_Cursor) return Nodes.Timed_Nodes.Deliverables.Deliverable_Access
   is
   begin
      if Index.Clone = 0 then
         return Deliverable_Vectors.Element (Index.Top_Level);
      else
         return Deliverable_Vectors.Element (Index.Top_Level).Clone (Index.Clone);
      end if;
   end Element;

   ---------------------
   -- Dependency_List --
   ---------------------

   overriding function Dependency_List (Item : Project_WP)
                                        return Node_Label_Lists.Vector
   is
      Result : Node_Label_Lists.Vector;
   begin
      for Tsk of Item.Task_List loop
         Result.Append (Tsk);
      end loop;

      return Result;
   end Dependency_List;


   ---------------------
   -- Add_Deliverable --
   ---------------------

   procedure Add_Deliverable
     (WP          : in out Project_WP;
      item : Timed_Nodes.Deliverables.Deliverable_Access)
   is
   begin
      if Item.Is_Clone or Item.Index /= Timed_Nodes.Deliverables.No_Deliverable
      then
         raise Constraint_Error;
      end if;

      WP.Deliverables.Append (Item);

      Item.Set_Index (WP.Deliverables.Last_Index);
   end Add_Deliverable;
   ------------
   -- Create --
   ------------

   function Create
     (Label        : WP_Label;
      Name         : String;
      Short_Name   : String;
      Leader       : Partners.Partner_Label;
      Objectives   : String;
      Description  : String;
      Active_When  : Action_Time;
      Depends_On   : Node_Label_Lists.Vector;
      Node_Dir     : in out Node_Tables.Node_Table)

      return Project_WP_Access
   is
      Result : Project_WP_Access;
   begin
      Result := new
        Project_WP'(Controlled with
                      Label             => Node_Label (Label),
                    Raw_Time          => Active_When,
                    Name              => To_Unbounded_String (Name),
                    Short_Name        => Make_Short_Name (Short_Name, Name),
                    Index             => No_WP,
                    Description       => To_Unbounded_String (Description),
                    Attributes        => Attribute_Maps.Empty_Map,
                    Leader            => Leader,
                    Class             => WP_Node,
                    Objectives        => To_Unbounded_String (Objectives),
                    Depend_On         => Depends_On,
                    WP_Tasks          => Task_Vectors.Empty_Vector,
                    Start_Symbolic    => <>,
                    Stop_Symbolic     => <>,
                    Duration_Symbolic => <>,
                    Start_At          => <>,
                    Stop_At           => <>,
                    Elapsed_Time      => <>,
                    Start_Fixed       => False,
                    Stop_Fixed        => False,
                    Partner_Effort    => Efforts.Effort_Maps.Empty_Map,
                    Deliverables      => Deliverable_Vectors.Empty_Vector
                   );

      Node_Dir.Insert (ID   => Node_Label (Label),
                       Item => Node_Access (Result));

      Result.Initialize_Efforts (Node_Dir);

      return Result;
   end Create;


   --------------
   -- Add_Task --
   --------------

   procedure Add_Task (WP  : in out Project_WP;
                       Tsk : Tasks.Project_Task_Access)
   is
      use Efforts.Effort_Maps;
      use type Efforts.Person_Months;
   begin
      WP.WP_Tasks.Append (Tsk);
      Tsk.Set_Index (WP.WP_Tasks.Last_Index);

      for Pos in WP.Partner_Effort.Iterate loop
         WP.Partner_Effort (Pos) := WP.Partner_Effort (Pos) + Tsk.Effort_Of (Key (Pos));
      end loop;
   end Add_Task;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (WP   : in out Project_WP;
                        Idx  : WP_Index)
   is
   begin
      if WP.Index /= No_Index and WP.Index /= Node_Index (Idx) then
         raise Constraint_Error;
      else
         WP.Index := Node_Index (Idx);
      end if;
   end Set_Index;


   overriding procedure Parse_Raw_Expressions
     (Item : in out Project_WP;
      Vars : Times.Time_Expressions.Parsing.Symbol_Table)
   is

   begin
      if Item.Raw_Time.Stop = To_Unbounded_String ("*") then
         Item.Raw_Time.Stop := To_Unbounded_String (After (Item.Task_List, "end"));
      end if;

      Action_Node (Item).Parse_Raw_Expressions (Vars);
   end Parse_Raw_Expressions;


   ---------------
   -- Task_List --
   ---------------

   function Task_List (Item : Project_WP) return Node_Label_Lists.Vector
   is
      Result : Node_Label_Lists.Vector;
   begin
      for T in Item.All_Tasks loop
         Result.Append (Element (T).Label);
      end loop;

      return Result;
   end Task_List;
end EU_Projects.Nodes.Action_Nodes.WPs;


--     ----------
--     -- Find --
--     ----------
--
--     function Find
--       (Where : Project_WP;
--        Label : Identifiers.Identifier)
--        return Searchable_Nodes.Child_Value
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Find unimplemented");
--        raise Program_Error with "Unimplemented function Find";
--        return Find (Where, Label);
--     end Find;
--
--     ------------
--     -- Exists --
--     ------------
--
--     function Exists
--       (Where : Project_WP;
--        Label : Identifiers.Identifier)
--        return Searchable_Nodes.Child_Class
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Exists unimplemented");
--        raise Program_Error with "Unimplemented function Exists";
--        return Exists (Where, Label);
--     end Exists;

--     ---------------------
--     -- Add_Description --
--     ---------------------
--
--     procedure Add_Description
--       (WP          : in out Project_WP;
--        Description : String)
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Add_Description unimplemented");
--        raise Program_Error with "Unimplemented procedure Add_Description";
--     end Add_Description;
--
--     --------------------
--     -- Add_Objectives --
--     --------------------
--
--     procedure Add_Objectives
--       (WP          : in out Project_WP;
--        Objectives  : String)
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Add_Objectives unimplemented");
--        raise Program_Error with "Unimplemented procedure Add_Objectives";
--     end Add_Objectives;



--     --------------------
--     -- Add_Dependence --
--     --------------------
--
--     procedure Add_Dependence
--       (WP    : in out Project_WP;
--        Label : Identifiers.Identifier)
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Add_Dependence unimplemented");
--        raise Program_Error with "Unimplemented procedure Add_Dependence";
--     end Add_Dependence;

--     ---------------------
--     -- Summarize_Tasks --
--     ---------------------
--
--     procedure Summarize_Tasks (WP : in out Project_WP) is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Summarize_Tasks unimplemented");
--        raise Program_Error with "Unimplemented procedure Summarize_Tasks";
--     end Summarize_Tasks;

--     ---------------
--     -- Set_Times --
--     ---------------
--
--     procedure Set_Times
--       (WP       : in out Project_WP;
--        Start_On : Symbolic_Time;
--        End_On   : Symbolic_Time;
--        Duration : Symbolic_Time)
--     is
--     begin
--        --  Generated stub: replace with real body!
--        pragma Compile_Time_Warning (Standard.True, "Set_Times unimplemented");
--        raise Program_Error with "Unimplemented procedure Set_Times";
--     end Set_Times;

--  declare
--              Tmp   : Unbounded_String := To_Unbounded_String ("max(");
--           begin
--              for Pos in Item.All_Tasks loop
--                 if Tmp /= "max(" then
--                    Tmp := Tmp & ",";
--                 end if;
--
--                 Tmp := Tmp & To_String (Element (Pos).Label) & ".end";
--              end loop;
--
--              if Tmp = "max(" then
--                 raise Bad_Input with "End time '*' with no task";
--              end if;
--
--              Tmp := Tmp & ")";
--
--              Item.Raw_Time.Stop := Tmp;
--           end;
