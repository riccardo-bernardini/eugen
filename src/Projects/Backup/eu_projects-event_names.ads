with EU_Projects.Nodes.Timed_Nodes.Milestones;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;
with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Identifiers;


package EU_Projects.Event_Names is
   use EU_Projects.Nodes.Timed_Nodes;
   use EU_Projects.Nodes.Action_Nodes;

   function Milestone_Var (Label : Identifiers.Identifier)
                           return String;

   function Deliverable_Var (Parent_Task : Tasks.Project_Task_Access;
                             Label       : Identifiers.Identifier)
                             return String;

   type Event_Class is (Start_Time, End_Time, Duration_Time);

   function Task_Var (Parent_WP : WPs.WP_Label;
                      Label     : Tasks.Task_Label;
                      Event     : Event_Class)
                      return String;

   function WP_Var
     (Label     : WPs.WP_Label;
      Event     : Event_Class)
      return String;

end EU_Projects.Event_Names;
