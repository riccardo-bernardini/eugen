with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces;

with EU_Projects.Nodes.Action_Nodes;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;
with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Nodes.Partners;
with EU_Projects.Node_Tables;




package EU_Projects.Nodes.Action_Nodes.WPs is

   type Project_WP is
     new Action_Node
   --       and Searchable_Nodes.Searchable_Node
   with
     private;

   type Project_WP_Access is access all Project_WP;

   subtype WP_Index is Node_Index;
   No_WP : constant Extended_Node_Index := No_Index;

   type WP_Label is new Node_Label;

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
     with
       Post => Create'Result.Index = No_WP;


   procedure Set_Index (WP   : in out Project_WP;
                        Idx  : WP_Index)
     with Pre => Wp.Index = No_WP,
     Post => Wp.Index = Idx;

   overriding
   function Full_Index (Item     : Project_WP;
                        Prefixed : Boolean)
                        return String;


   overriding function Dependency_List (Item : Project_WP)
                                        return Node_Label_Lists.Vector;


   -- As many other "add" procedures, this procedure is in charge of assigning
   -- an index to the new task, unless the task has already an index (which
   -- must not be already present). New indexes are assigned incrementally.
   --
   -- This, basically, is the plain english "translation" of
   -- the pre/postconditions...
   procedure Add_Task (WP  : in out Project_WP;
                       Tsk : Tasks.Project_Task_Access)
     with
       Post => (
                (Tsk.Index /= Tasks.No_Task)
                and (WP.Contains (Tsk.Index))
                and Correctly_Updated (Old_Index => Tsk.Index'Old,
                                       New_Index => Tsk.Index,
                                       Old_Max   => WP.Max_Task_Index'Old,
                                       New_Max   => WP.Max_Task_Index)
               ),
       Pre => (Tsk.Index = Tasks.No_Task);

   -- Return the maximum of the indexes of the tasks currently included
   -- in the WP.  It is here since it is necessary for the post condition of
   -- Add_Task.
   function Max_Task_Index (WP : Project_WP) return Extended_Node_Index;

   procedure Add_Deliverable
     (WP          : in out Project_WP;
      Item        : Timed_Nodes.Deliverables.Deliverable_Access)
     with Pre =>
       not Item.Is_Clone
       and Item.Index = Timed_Nodes.Deliverables.No_Deliverable,
       Post =>
         Item.Index /= Timed_Nodes.Deliverables.No_Deliverable;

   function Contains (WP  : Project_WP;
                      Idx : Tasks.Task_Index)
                      return Boolean;




   type Task_Cursor is private;

   function Has_Element (X : Task_Cursor) return Boolean;

   package Task_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Task_Cursor,
                                  Has_Element  => Has_Element);


   type Task_Iterator is
     new Task_Iterator_Interfaces.Forward_Iterator
   with private;

   overriding
   function First (Object : Task_Iterator) return Task_Cursor;

   overriding
   function Next
     (Object   : Task_Iterator;
      Position : Task_Cursor) return Task_Cursor;

   function All_Tasks
     (Item : Project_WP)
      return Task_Iterator_Interfaces.Forward_Iterator'Class;

   function Element (Index : Task_Cursor)
                     return Tasks.Project_Task_Access;

   function Task_List (Item : Project_WP) return Node_Label_Lists.Vector;


   ---------------------------
   -- Deliverable Iterators --
   ---------------------------

   type Deliverable_Cursor is private;

   function Has_Element (X : Deliverable_Cursor) return Boolean;

   package Deliverable_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Deliverable_Cursor,
                                  Has_Element  => Has_Element);

   type Deliverable_Iterator is
     new Deliverable_Iterator_Interfaces.Forward_Iterator
   with private;


   overriding
   function First (Object : Deliverable_Iterator) return Deliverable_Cursor;

   overriding
   function Next
     (Object   : Deliverable_Iterator;
      Position : Deliverable_Cursor) return Deliverable_Cursor;


   function All_Deliverables
     (Item : Project_WP)
      return Deliverable_Iterator_Interfaces.Forward_Iterator'Class;

   function Element (Index : Deliverable_Cursor)
                     return Nodes.Timed_Nodes.Deliverables.Deliverable_Access;

   overriding procedure Parse_Raw_Expressions
     (Item : in out Project_WP;
      Vars : Times.Time_Expressions.Parsing.Symbol_Table);

private
   use Tasks;
   use type Timed_Nodes.Deliverables.Deliverable_Access;

   package WP_Index_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => WP_Index);

   package Deliverable_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Timed_Nodes.Deliverables.Deliverable_Index,
                                 Element_Type => Timed_Nodes.Deliverables.Deliverable_Access);
   ---------------------
   --  Task iteration --
   ---------------------

   package Task_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Tasks.Task_Index,
                                 Element_Type => Tasks.Project_Task_Access);


   type Task_Cursor is new Task_Vectors.Cursor;

   type Task_Iterator is
     new Task_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Task_Vectors.Cursor;
      end record;

   overriding
   function First (Object : Task_Iterator) return Task_Cursor
   is (Task_Cursor (Object.Start));

   overriding
   function Next
     (Object   : Task_Iterator;
      Position : Task_Cursor) return Task_Cursor
   is (Task_Cursor (Task_Vectors.Next (Task_Vectors.Cursor (Position))));


   function Element (Index : Task_Cursor) return Tasks.Project_Task_Access
   is (Task_Vectors.Element (Task_Vectors.Cursor (Index)));


   function Has_Element (X : Task_Cursor) return Boolean
   is (Task_Vectors.Has_Element (Task_Vectors.Cursor (X)));


   ----------------
   -- Finally... --
   ----------------
   type Project_WP is
     new Nodes.Action_Nodes.Action_Node
   with
      record
         Objectives       : Unbounded_String;
         Depend_On        : Node_Label_Lists.Vector;
         WP_Tasks         : Task_Vectors.Vector;
         Deliverables     : Deliverable_Vectors.Vector;
      end record;
   --
   --     function Index (WP : Project_WP) return Extended_Node_Index
   --     is (WP_Index (WP.Index));

   function Contains (WP : Project_WP; Idx : Tasks.Task_Index) return Boolean
   is ((Idx >= WP.WP_Tasks.First_Index and Idx <= WP.WP_Tasks.Last_Index)
       and then (WP.WP_Tasks (Idx) /= null));

   ----------------------------
   --  Deliverable iteration --
   ----------------------------

   type Deliverable_Cursor is
      record
         Top_Level : Deliverable_Vectors.Cursor;
         Clone     : Natural;
      end record;

   type Deliverable_Iterator is
     new Deliverable_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Deliverable_Vectors.Cursor;
      end record;


   overriding
   function First (Object : Deliverable_Iterator) return Deliverable_Cursor
   is (Deliverable_Cursor'(Top_Level => Object.Start, Clone => 0));


   function Has_Element (X : Deliverable_Cursor) return Boolean
   is (Deliverable_Vectors.Has_Element (X.Top_Level));


   function All_Deliverables (Item : Project_WP)
                              return Deliverable_Iterator_Interfaces.Forward_Iterator'Class
   is (Deliverable_Iterator'(Start => Item.Deliverables.First));



   overriding
   function Full_Index (Item     : Project_WP;
                        Prefixed : Boolean)
                        return String
   is ((if Prefixed then "WP" else "") & Chop (Node_Index'Image (Item.Index)));

   function Max_Task_Index (WP : Project_WP) return Extended_Node_Index
   is (WP.WP_Tasks.Last_Index);

   function All_Tasks (Item : Project_WP)
                       return Task_Iterator_Interfaces.Forward_Iterator'Class
   is (Task_Iterator'(Start => Item.WP_Tasks.First));




   --     function All_Deliverables
   --       (Item : Project_WP)
   --        return Deliverable_Iterator_Interfaces.Forward_Iterator'Class
   --     is (Deliverable_Iterator'(Start => Item.WP_Deliverables.First));
   --
   --
   --     function All_Milestones
   --       (Item : Project_WP)
   --        return Milestone_Iterator_Interfaces.Forward_Iterator'Class
   --     is (Milestone_Iterator'(Start => Item.WP_Milestones.First));

end EU_Projects.Nodes.Action_Nodes.WPs;

--     procedure Add_Deliverable (WP          : in out Project_WP;
--                                Deliverable : Timed_Nodes.Deliverables.Deliverable_Access);
--
--     procedure Add_Dependence (WP    : in out Project_WP;
--                               Label : Identifiers.Identifier);
--
--     procedure Summarize_Tasks (WP : in out Project_WP);

--     function Find (Where : Project_WP;
--                    Label : Identifiers.Identifier)
--                    return Searchable_Nodes.Child_Value;
--     --  Search for a child with the given name and return it
--     --  Return No_Child if the child does not exist
--
--     function Exists (Where : Project_WP;
--                      Label : Identifiers.Identifier)
--                      return Searchable_Nodes.Child_Class;


