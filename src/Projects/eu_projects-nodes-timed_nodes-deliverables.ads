
with EU_Projects.Nodes.Timed_Nodes;
with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Node_Tables;

package EU_Projects.Nodes.Timed_Nodes.Deliverables is
   subtype Deliverable_Index is Node_Index;
   No_Deliverable : constant Extended_Node_Index := No_Index;

   type Deliverable_Label is new Node_Label;

   type Deliverable is new Nodes.Timed_Nodes.Timed_Node with private;

   type Deliverable_Access is  access all Deliverable;
   type Deliverable_Type is (Report, Demo, Dissemination, Other);
   type Dissemination_Level is (Public, Confidential, Classified);
   type Deliverable_Status is (Parent, Clone, Stand_Alone);


   overriding function Dependency_Ready_Var (Item : Deliverable) return String
   is ("end");

   function Status (Item : Deliverable) return Deliverable_Status;

   function Create (Label             : Deliverable_Label;
                    Name              : String;
                    Description       : String;
                    Short_Name        : String;
                    Delivered_By      : Node_Label_Lists.Vector;
                    Due_On            : String;
                    Node_Dir          : in out Node_Tables.Node_Table;
                    Parent_WP         : Node_Access;
                    Linked_Milestones : Nodes.Node_Label_Lists.Vector;
                    Deliv_Type        : Deliverable_Type;
                    Dissemination     : Dissemination_Level)
                    return Deliverable_Access
     with Post => Create'Result.Status = Stand_Alone;

   overriding function Variables (Item : Deliverable) return Variable_List
   is ((if Item.Status = Parent then Empty_List else (1 => Event_Names.Event_Time_Name)));


   overriding function Is_Variable (Item : Deliverable;
                                    Var  : Simple_Identifier) return Boolean
   is (if Item.Status = Parent then False else Var = Event_Names.Event_Time_Name);

   procedure Clone (Item      : in out Deliverable;
                    Due_On    : in     String;
                    Node_Dir  : in out Node_Tables.Node_Table)
     with
       Pre =>
         not Item.Is_Clone
         and Item.Index = No_Deliverable,
         Post =>
           Item.Status = Parent
           and Item.Max_Clone = Item.Max_Clone'Old +
             (if Item.Status'Old = Stand_Alone then 2 else   1);

   function Clone_Sub_Label (Item : Deliverable) return String;


   function Clone (Item : Deliverable; Idx : Positive) return Deliverable_Access;

   function Is_Clone (Item : Deliverable) return Boolean
   is (Item.Status = Clone);

   overriding function Due_On (Item : Deliverable) return Times.Instant
     with Pre => Item.Time_Fixed and Item.Status /= Parent;

   use type Times.Instant;

   package Instant_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Times.Instant);

   function Due_On (Item   : Deliverable;
                    Sorted : Boolean := True) return Instant_Vectors.Vector;

   function Image (Item      : Instant_Vectors.Vector;
                   Separator : String := ", ") return String;

   procedure Set_Index (Item : in out Deliverable;
                        Idx  : Deliverable_Index)
     with Pre => Item.Index = No_Deliverable;

   overriding
   function Full_Index (Item     : Deliverable;
                        Prefixed : Boolean)
                        return String;

   function Max_Clone (Item : Deliverable) return Natural;

   function Delivered_By (Item : Deliverable)
                          return Nodes.Node_Label_Lists.Vector;

   function Dependency_List (Item : Deliverable)
                             return Node_Label_Lists.Vector
   is (Item.Delivered_By);

   function Linked_Milestones (Item : Deliverable)
                               return Nodes.Node_Label_Lists.Vector;

   function Parent_Wp (Item : Deliverable) return Node_Access;

   function Nature (Item : Deliverable) return Deliverable_Type;

   function Dissemination (Item : Deliverable) return Dissemination_Level;
private
   --     use Tasks;
   --
   --     package Task_Lists is
   --       new Ada.Containers.Vectors (Index_Type   => Positive,
   --                                   Element_Type => Project_Task_Access);

   subtype Clone_Index_Type is Integer range 0 .. Character'Pos ('z')-Character'Pos ('a');
   No_Clone : constant Clone_Index_Type := 0;

   package Deliverable_Arrays is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Deliverable_Access);


   type Deliverable is
     new Nodes.Timed_Nodes.Timed_Node with
      record
         Deliverer         : Node_Label_Lists.Vector;
         Linked_Milestones : Nodes.Node_Label_Lists.Vector;
         Parent_WP         : Node_Access;
         Deliv_Type        : Deliverable_Type;
         Dissemination     : Dissemination_Level;
         Status            : Deliverable_Status;
         Clone_List        : Deliverable_Arrays.Vector;
         Clone_Index       : Clone_Index_Type;
      end record
     with
       Dynamic_Predicate => (if Deliverable.Status /= Parent
                               then
                                 Deliverable.Clone_List.Is_Empty else
                               (for all X of Deliverable.Clone_List => X.Status = Clone));

   function Status (Item : Deliverable) return Deliverable_Status
   is (Item.Status);


   function Clone (Item : Deliverable; Idx : Positive) return Deliverable_Access
   is (Item.Clone_List (Idx));


   function Max_Clone (Item : Deliverable) return Natural
   is (if Item.Clone_List.Is_Empty then 0 else Item.Clone_List.Last_Index);

   function Delivered_By (Item : Deliverable)
                          return Nodes.Node_Label_Lists.Vector
   is (Item.Deliverer);

   function Parent_Wp (Item : Deliverable) return Node_Access
   is (Item.Parent_Wp);

   function Clone_Sub_Label (Item : Deliverable) return String
   is (if Item.Status = Clone
       then "." & Character'Val (Character'Pos ('a')-1 + Item.Clone_Index)
       else "");

   overriding
   function Full_Index (Item     : Deliverable;
                        Prefixed : Boolean)
                        return String
   is ((if Prefixed then "D" else "")
       & Chop (Node_Index'Image (Item.Parent_WP.Index))
       & "."
       & Chop (Node_Index'Image (Item.Index))
       & Item.Clone_Sub_Label);

   function Linked_Milestones (Item : Deliverable)
                               return Nodes.Node_Label_Lists.Vector
   is (Item.Linked_Milestones);


   function Nature (Item : Deliverable) return Deliverable_Type
   is (Item.Deliv_Type);

   function Dissemination (Item : Deliverable) return Dissemination_Level
   is (Item.Dissemination);

   --     function Index (Item : Deliverable) return Deliverable_Index
   --     is (Deliverable_Index (Item.Index));

end EU_Projects.Nodes.Timed_Nodes.Deliverables;
