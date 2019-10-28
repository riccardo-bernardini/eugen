
with DOM.Core.Documents;
with Project_Processor.Parsers.Parser_Tables;

with XML_Utilities;
with XML_Scanners;
with DOM.Core.Nodes;

with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Timed_Nodes.Milestones;

with Project_Processor.Parsers.XML_Parsers.Sub_Parsers;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;

package body Project_Processor.Parsers.XML_Parsers is
   use XML_Scanners;
   use EU_Projects.Projects;

   ------------
   -- Create --
   ------------

   function Create
   (Params : not null access Plugins.Parameter_Maps.Map)
   return Parser_Type
   is
      pragma Unreferenced (Params);
      Result : Parser_Type;
   begin
      return Result;
   end Create;


   procedure Parse_Project
         (Parser    : in out Parser_Type;
          Project   :    out Project_Descriptor;
          N         : in     DOM.Core.Node)
   is
   pragma Unreferenced (Parser);
      use EU_Projects.Nodes;

      Scanner           : XML_Scanner := Create_Child_Scanner (N);


      procedure Add_Partner (N : DOM.Core.Node) is
      begin
         Project.Add_Partner (Sub_Parsers.Parse_Partner (N));
      end Add_Partner;

      procedure Add_Milestone (N : DOM.Core.Node) is
         Milestone : Timed_Nodes.Milestones.Milestone_Access;
      begin
         Sub_Parsers.Parse_Milestone (N, Milestone);

         Project.Add_Milestone (Milestone);
      end Add_Milestone;

      procedure Add_WP (N : DOM.Core.Node) is

         WP : Action_Nodes.WPs.Project_WP_Access;
      begin
         Sub_Parsers.Parse_WP (N, WP);

         Project.Add_WP (WP);
      end Add_WP;

      procedure Handle_Configuration (N : DOM.Core.Node) is
      begin
         Sub_Parsers.Parse_Configuration (Project, N);
      end Handle_Configuration;

      procedure Add_Risk (N : DOM.Core.Node) is
      begin
         Project.Add_Risk (Sub_Parsers.Parse_Risk (N));
      end Add_Risk;

      procedure Add_Deliverable (N : DOM.Core.Node) is
         Deliv : Timed_Nodes.Deliverables.Deliverable_Access;
      begin
         Sub_Parsers.Parse_Deliverable (N, Deliv);

         Project.Add_Deliverable (Deliv);
      end Add_Deliverable;
   begin
      if DOM.Core.Nodes.Node_Name (N) /= "project" then
         raise Parsing_Error;
      end if;

      Dom.Core.Nodes.Print (N);

      Scanner.Parse_Optional ("configuration", Handle_Configuration'Access);

      Scanner.Parse_Sequence ("partner", Add_Partner'Access);
      Scanner.Parse_Sequence ("wp", Add_WP'Access);
      Scanner.Parse_Sequence ("milestone", Add_Milestone'Access);

      Scanner.Parse_Sequence (Name       => "deliverable",
                              Callback   => Add_Deliverable'Access,
                              Min_Length => 0);


      Scanner.Parse_Sequence (Name       => "risk",
                              Callback   => Add_Risk'Access,
                              Min_Length => 0);
   end Parse_Project;

   -----------
   -- Parse --
   -----------

   procedure Parse
         (Parser    : in out Parser_Type;
          Project   :    out EU_Projects.Projects.Project_Descriptor;
          Input     : String)
   is
      use DOM.Core.Documents;
   begin
      Parse_Project
            (Parser    => Parser,
             Project   => Project,
             N         => Get_Element (XML_Utilities.Parse_String (Input)));

      Project.Freeze;
   end Parse;


begin
   Parser_Tables.Register (ID  => "xml",
                           Tag => Parser_Type'Tag);
end Project_Processor.Parsers.XML_Parsers;
