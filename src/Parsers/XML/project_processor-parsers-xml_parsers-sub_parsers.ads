with EU_Projects.Nodes.Partners;
--  with EU_Projects.Identifiers;
--  with EU_Projects.Costs;
with EU_Projects.Nodes.Timed_Nodes.Milestones;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;
with EU_Projects.Nodes.Risks;

with DOM.Core.Nodes;

private package Project_Processor.Parsers.XML_Parsers.Sub_Parsers is
   use EU_Projects.Projects;
   use EU_Projects;
   use EU_Projects.Nodes;

   procedure Parse_Configuration (Project : in out Project_Descriptor;
                                  Input   : in     DOM.Core.Node)
     with Pre => DOM.Core.Nodes.Node_Name (Input) = "configuration";


   function Parse_Partner
   (N     : DoM.Core.Node)
   return  EU_Projects.Nodes.Partners.Partner_Access;

   procedure Parse_Milestone
   (N         : in     DoM.Core.Node;
   Milestone :    out Nodes.Timed_Nodes.Milestones.Milestone_Access);

   procedure Parse_Deliverable
     (Deliv_Node  : in     DOM.Core.Node;
      Deliv       :    out Nodes.Timed_Nodes.Deliverables.Deliverable_Access);

   procedure Parse_Task
   (Task_Node    : in     DOM.Core.Node;
          Parent_WP    : in     Nodes.Action_Nodes.WPs.Project_WP_Access;
          Descriptor   :    out Nodes.Action_Nodes.Tasks.Project_Task_Access);

   procedure Parse_WP
     (WP_Node   : in     DOM.Core.Node;
      WP        :    out Nodes.Action_Nodes.WPs.Project_WP_Access);

   function Parse_Risk (Risk_Node : DOM.Core.Node)
                        return Nodes.Risks.Risk_Access;

end Project_Processor.Parsers.XML_Parsers.Sub_Parsers;
