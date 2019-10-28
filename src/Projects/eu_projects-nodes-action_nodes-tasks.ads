
with EU_Projects.Nodes.Action_Nodes;
with EU_Projects.Nodes.Partners;
with EU_Projects.Node_Tables;


package EU_Projects.Nodes.Action_Nodes.Tasks is

   subtype Task_Index is  Node_Index;
   No_Task : constant Extended_Node_Index := No_Index;

   type Task_Label is new Node_Label;

   type Project_Task is
     new Nodes.Action_Nodes.Action_Node
   with
     private;


   type Project_Task_Access is access Project_Task;

   subtype Task_Intensity is Float  range 0.0 .. 1.0;

   function Image (X : Task_Intensity) return String
   is (Task_Intensity'Image(X));

   function Value (X : String) return Task_Intensity;

   function Create
     (Label        : Task_Label;
      Name         : String;
      Short_Name   : String;
      Leader       : Partners.Partner_Label;
      Parent_WP    : Node_Access;
      Description  : String;
      Active_When  : Action_Time;
      Depends_On   : Nodes.Node_Label_Lists.Vector;
      Intensity    : Task_Intensity;
      Node_Dir     : in out Node_Tables.Node_Table)
      return Project_Task_Access
     with Post => Create'Result.Index = No_Task;

   --     function Index (Item : Project_Task) return Task_Index;



   procedure Set_Index (Tsk : in out Project_Task;
                        Idx : Task_Index)
     with
       Pre => Tsk.Index = No_Task,
       Post => Tsk.Index = Idx;

   function Intensity (Tsk : Project_Task) return Task_Intensity;

   overriding
   function Full_Index (Item     : Project_Task;
                        Prefixed : Boolean)
                        return String;

   procedure Add_Dependence (Item : in out Project_Task;
                             Dep  : in     Task_Label);

   function Dependency_List (Item : Project_Task)
                             return Node_Label_Lists.Vector;





   --
   -- I use a postcondition to ensure that the access value
   -- refers to a WP. I need to resort to this form of "weak strong typing"
   -- in order to avoid circular dependencies.  See comments in Nodes.
   --
   function Parent_WP (Item : Project_Task) return Nodes.Node_Access
     with Post => Parent_WP'Result.Class = WP_Node;
private
   --     package Dependence_Vectors is
   --       new Ada.Containers.Vectors (Index_Type   => Positive,
   --                                   Element_Type => Task_Label);

   type Project_Task is
     new Nodes.Action_Nodes.Action_Node
   with
      record
         Depend_On      : Node_Label_Lists.Vector;
         Parent         : Node_Access;
         Intensity      : Task_Intensity;
      end record;


   function Dependency_List (Item : Project_Task)
                             return Node_Label_Lists.Vector
   is (Item.Depend_On);

   function Parent_WP (Item : Project_Task) return Nodes.Node_Access
   is (Item.Parent);

   overriding
   function Full_Index (Item     : Project_Task;
                        Prefixed : Boolean)
                        return String
   is ((if Prefixed then "T" else "")
       & Chop (Node_Index'Image (Item.Parent_WP.Index))
       & "."
       & Chop (Node_Index'Image (Item.Index)));


   function Intensity (Tsk : Project_Task) return Task_Intensity
   is (Tsk.Intensity);

   --
   --     function Index (Item : Project_Task) return Task_Index
   --     is (Task_Index (Item.Index));

   --     function Effort_Of (Item    : Project_Task;
   --                         Partner : Nodes.Partners.Partner_Label)
   --                         return Efforts.Person_Months
   --     is (if Item.Partner_Effort.Contains (Partner) then
   --            Item.Partner_Effort (Partner)
   --         else
   --            raise Unknown_Partner);

end EU_Projects.Nodes.Action_Nodes.Tasks;
