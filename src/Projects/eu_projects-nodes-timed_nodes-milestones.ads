
with EU_Projects.Nodes.Timed_Nodes;
with EU_Projects.Node_Tables;


package EU_Projects.Nodes.Timed_Nodes.Milestones is
   subtype Milestone_Index is Node_Index;
   No_Milestone : constant Extended_Node_Index := No_Index;

   type Milestone is new Nodes.Timed_Nodes.Timed_Node with private;

   type Milestone_Label is new Node_Label;

   type Milestone_Access is access  all Milestone;


   function Create (Label        : Milestone_Label;
                    Name         : String;
                    Short_Name   : String;
                    Description  : String;
                    Due_On       : String;
                    Node_Dir     : in out Node_Tables.Node_Table;
                    Verification : String)
                    return Milestone_Access;

   --     function Index (Item : Milestone) return Milestone_Index;

   procedure Set_Index (Item : in out Milestone;
                        Idx  : Milestone_Index);

   procedure Update_Index (Item : in out Milestone;
                           Idx  : Milestone_Index);

   overriding
   function Full_Index (Item     : Milestone;
                        Prefixed : Boolean)
                        return String;

   procedure Add_Deliverable (Item  : in out Milestone;
                              Deliv : Node_Label);

   function Deliverable_List (Item : Milestone) return Node_Label_Lists.Vector;

   overriding function Dependency_List (Item : Milestone)
                                        return Node_Label_Lists.Vector
   is (Item.Deliverable_List);

   overriding function Dependency_Ready_Var (Item : Milestone) return String
   is ("when");


   function Verification_Mean (Item : Milestone) return String;

private
   type Milestone is new Nodes.Timed_Nodes.Timed_Node with
      record
         Deliv : Node_Label_Lists.Vector;
         Verification : Unbounded_String;
      end record;

   --     function Index (Item : Milestone) return Milestone_Index
   --     is (Milestone_Index (Item.Index));

   overriding
   function Full_Index (Item     : Milestone;
                        Prefixed : Boolean)
                        return String
   is ((if Prefixed then "M" else "") & Image (Item.Index));

   function Deliverable_List (Item : Milestone) return Node_Label_Lists.Vector
   is (Item.Deliv);


   function Verification_Mean (Item : Milestone) return String
   is (To_String (Item.Verification));
end EU_Projects.Nodes.Timed_Nodes.Milestones;
