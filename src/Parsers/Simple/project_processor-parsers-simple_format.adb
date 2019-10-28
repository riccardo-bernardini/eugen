pragma Ada_2012;
with Line_Arrays;
with Node_List_Parsers;

with EU_Projects.Times.Time_Expressions;  use EU_Projects.Times.Time_Expressions;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Partners;

with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Efforts;
with EU_Projects.Nodes.Timed_Nodes;
with EU_Projects.Nodes.Timed_Nodes.Milestones;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;
with EU_Projects.Event_Names;

with Project_Processor.Parsers.Parser_Tables;

with Ada.Text_IO; use Ada.Text_IO;
with EU_Projects.Node_Tables;
with EU_Projects.Nodes.Timed_Nodes.Project_Infos;

with Tokenize.Token_Vectors;
--  with Eu_Projects.Event_Names;

package body Project_Processor.Parsers.Simple_Format is
   use EU_Projects;
   use EU_Projects.Nodes.Timed_Nodes;

   type Node_Class is (Metadata, Wp, Partner, Milestone, Deliverable, Tsk, Role);

   subtype Top_Level_Node is Node_Class range Metadata .. Milestone;

   package Node_Parser is
     new Node_List_Parsers (Node_Class, EU_Projects.Bounded_Identifiers);

   use Node_Parser;


   function Deliverable_Type_Check is
     new Generic_Enumerative (Deliverables.Deliverable_Type);


   function Dissemination_Level_Check is
     new Generic_Enumerative (Deliverables.Dissemination_Level);

   Basic_Checker : constant Attribute_Checker :=
                     Mandatory ("label") +
                     Mandatory ("name") +
                     Mandatory ("short");

   Project_Checker : constant Attribute_Checker :=
                       Basic_Checker + Default ("end", Event_Names.Default_Time);

   Event_Checker       : constant Attribute_Checker :=
                           Basic_Checker + Default ("when", "default");

   Activity_Checker    : constant Attribute_Checker :=
                           Basic_Checker
                             + Mandatory ("begin")
                           + Alternative ("end|duration")
                           + Default ("objective", "")
                           + Default ("dependencies", "");

   WP_Checker          : constant Attribute_Checker := Activity_Checker + Mandatory ("leader");

   Task_Checker        : constant Attribute_Checker := WP_Checker
                           + Mandatory ("effort")
                           + Default ("intensity", "100%");

   Milestone_Checker   : constant Attribute_Checker := Event_Checker
                           + Default ("verification", "");
--                             + Default ("deliverables", "");

   Deliverable_Checker : constant Attribute_Checker := Event_Checker
                           + Default ("tasks", "")
                           + Default ("milestones", "")
                           + Deliverable_Type_Check ("type")
                           + Dissemination_Level_Check ("dissemination");

   Partner_Checker     : constant Attribute_Checker := Basic_Checker
                           + Mandatory ("country");

   Role_Checker : constant Attribute_Checker := Mandatory ("name")
                    + Default ("monthly-cost", "0")
                    + Default ("description", "");

   function Get_Times (Node : Node_Type) return Nodes.Action_Nodes.Action_Time
   is
      use Nodes.Action_Nodes;

      End_Given      : constant Boolean := Node.Contains (+"end");
      Duration_Given : constant Boolean := Node.Contains (+"duration");
   begin

      if End_Given and Duration_Given then
         -- This should never happen
         raise Program_Error with "end and duration together";

      elsif (not End_Given) and (not Duration_Given) then
         -- This neither
         raise Program_Error with "neither end nor duration";

      elsif End_Given and (not Duration_Given) then
         --           Put_Line (Node ("begin") & "|" & Node ("end"));

         return Create (Start_On => Instant_Raw (Node ("begin")),
                        End_On   => Instant_Raw (Node ("end")));

      else
         return Create (Start_On => Instant_Raw (Node.Value ("begin")),
                        Duration => Duration_Raw (Node.Value ("duration")));
      end if;
   end Get_Times;

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

   procedure Parse_Deliverable (WP       :        EU_Projects.Nodes.Action_Nodes.WPs.Project_WP_Access;
                                Node_Seq : in out Node_Parser.Node_Scanner;
                                Node_Dir : in out Node_Tables.Node_Table)
     with Pre => Class_Is (Node_Seq, Deliverable);

   -----------------------
   -- Parse_Deliverable --
   -----------------------

   -----------------------
   -- Parse_Deliverable --
   -----------------------

   procedure Parse_Deliverable (WP       :        EU_Projects.Nodes.Action_Nodes.WPs.Project_WP_Access;
                                Node_Seq : in out Node_Parser.Node_Scanner;
                                Node_Dir : in out Node_Tables.Node_Table)
   is
      use EU_Projects.Nodes.Timed_Nodes.Deliverables;

      Deliv_Node : Node_Type := Peek (Node_Seq);
      This_Deliv : Deliverable_Access;
   begin
      Next (Node_Seq);
      Check (Deliverable_Checker, Deliv_Node);

      declare
         use Tokenize;
         Label : constant Deliverable_Label :=
                   Deliverable_Label'(To_ID (Deliv_Node ("label")));

         Delivered_By : constant EU_Projects.Nodes.Node_Label_Lists.Vector :=
                          Nodes.Parse_Label_List (Deliv_Node ("tasks"));

         Due_Times    : Token_Vectors.Vector :=
                          Token_Vectors.To_Vector (Split (Deliv_Node ("when"), ';'));

      begin
         if Node_Dir.Contains (EU_Projects.Nodes.Node_Label (Label)) then
            raise Parsing_Error
              with "Duplicated label '" & Image (Label) & "' in deliverable";
         end if;


         This_Deliv :=
           Create (Label             => Label,
                   Name              => Deliv_Node ("name"),
                   Short_Name        => Deliv_Node ("short"),
                   Description       => Deliv_Node.Description,
                   Delivered_By      => Delivered_By,
                   Linked_Milestones => Nodes.Parse_Label_List (Deliv_Node ("milestones")),
                   Due_On            => Due_Times.First_Element,
                   Node_Dir          => Node_Dir,
                   Parent_WP         => EU_Projects.Nodes.Node_Access (WP),
                   Deliv_Type        => Deliverable_Type'Value (Deliv_Node ("type")),
                   Dissemination     => Dissemination_Level'Value (Deliv_Node ("dissemination")));

         for Idx in Due_Times.First_Index + 1 .. Due_Times.Last_Index loop
            Clone (Item      => This_Deliv.all,
                   Due_On    => Due_Times (Idx),
                   Node_Dir  => Node_Dir);

         end loop;

         WP.Add_Deliverable (This_Deliv);
      end;

   end Parse_Deliverable;

   procedure Parse_Task (Parent   : Nodes.Action_Nodes.WPs.Project_WP_Access;
                         Nodes    : in out Node_Parser.Node_Scanner;
                         Node_Dir : in out Node_Tables.Node_Table)
     with Pre => Class_Is (Nodes, Tsk);

   procedure Parse_Task (Parent   : Nodes.Action_Nodes.WPs.Project_WP_Access;
                         Nodes    : in out Node_Parser.Node_Scanner;
                         Node_Dir : in out Node_Tables.Node_Table)
   is
      use EU_Projects.Nodes.Action_Nodes.Tasks;
      use EU_Projects.Nodes.Partners;

      Task_Node : Node_Type := Peek (Nodes);
      This_Task : Project_Task_Access;
   begin
      Check (Task_Checker, Task_Node);

      Next (Nodes);

      This_Task := Create
        (Label       => Task_Label (EU_Projects.To_ID (Task_Node.Value ("label"))),
         Name        => Task_Node.Value ("name"),
         Short_Name  => Task_Node.Value ("short"),
         Leader      => Partner_Label (EU_Projects.To_ID (Task_Node.Value ("leader"))),
         Description => Line_Arrays.Join (Task_Node.Description),
         Active_When => Get_Times (Task_Node),
         Parent_WP   => EU_Projects.Nodes.Node_Access (Parent),
         Intensity   => Value (Task_Node.Value ("intensity")),
         Depends_On  => EU_Projects.Nodes.Parse_Label_List (Task_Node.Value ("dependencies")),
         Node_Dir    => Node_Dir);

      declare
         use EU_Projects.Efforts.Effort_Maps;
      begin
         for Pos in EU_Projects.Efforts.Parse (Task_Node ("effort")).Iterate loop
            This_Task.Add_Effort (Partner => Key (Pos),
                                  PM      => Element (Pos));
         end loop;
      exception
         when EU_Projects.Efforts.Bad_Effort_List =>
            Put_Line (Standard_Error, "Bad effort list for task '" & Task_Node.Value ("name") & "'");

            raise;
      end;

      Parent.Add_Task (This_Task);
   end Parse_Task;

   procedure Parse_WP (Nodes   : in out Node_Parser.Node_Scanner;
                       Project :    out EU_Projects.Projects.Project_Descriptor)
     with Pre => Peek_Class (Nodes) = Wp;


   procedure Parse_WP (Nodes   : in out Node_Parser.Node_Scanner;
                       Project :    out EU_Projects.Projects.Project_Descriptor)
   is
      use EU_Projects.Nodes.Action_Nodes.WPs;
      use EU_Projects.Nodes.Partners;

      WP_Node : Node_Type := Peek (Nodes);

      use EU_Projects.Nodes;

      This_Wp  : Project_WP_Access;
   begin
      Check (WP_Checker, Wp_Node);

      if  Project.Find (To_ID (WP_Node ("leader"))) = null then
         raise Bad_Input with "Leader '" & WP_Node ("leader") & "' Unknown";
      end if;

      This_Wp := Create
        (Label       => WP_Label (EU_Projects.To_ID (WP_Node.Value ("label"))),
         Name        => WP_Node.Value ("name"),
         Short_Name  => WP_Node.Value ("short"),
         Leader      => Partner_Label (EU_Projects.To_ID (WP_Node.Value ("leader"))),
         Objectives  => WP_Node.Value ("objective"),
         Description => Line_Arrays.Join (WP_Node.Description),
         Active_When => Get_Times (WP_Node),
         Depends_On  => EU_Projects.Nodes.Parse_Label_List (Wp_Node.Value ("dependencies")),
         Node_Dir    => Project.Node_Directory);


      Next (Nodes);

      while Class_Is (Nodes, Tsk) loop
         Parse_Task (This_Wp, Nodes, Project.Node_Directory);
      end loop;

      while Class_Is (Nodes, Deliverable) loop
         Parse_Deliverable (This_Wp, Nodes, Project.Node_Directory);
      end loop;

      Project.Add_WP (This_Wp);
   end Parse_WP;

   procedure Parse_Role  (Nodes   : in out Node_Parser.Node_Scanner;
                          Partner :    EU_Projects.Nodes.Partners.Partner_Access)
     with Pre => Class_Is (Nodes, Role);

   procedure Parse_Role  (Nodes   : in out Node_Parser.Node_Scanner;
                          Partner :    EU_Projects.Nodes.Partners.Partner_Access)
   is
      use EU_Projects.Nodes.Partners;

      Role_Node : Node_Type := Peek (Nodes);
   begin
      Next (Nodes);

      Check (Role_Checker, Role_Node);

      Partner.Add_Role (Role         => Role_Name'(To_Id (Role_Node ("name"))),
                        Description  => Role_Node ("description"),
                        Monthly_Cost => Currency'Value (Role_Node ("monthly-cost")));
   end Parse_Role;

   procedure Parse_Partner  (Nodes   : in out Node_Parser.Node_Scanner;
                             Project :    out EU_Projects.Projects.Project_Descriptor)
     with Pre => Class_Is (Nodes, Partner);

   procedure Parse_Partner  (Nodes   : in out Node_Parser.Node_Scanner;
                             Project :    out EU_Projects.Projects.Project_Descriptor)
   is
      use EU_Projects.Nodes.Partners;

      Partner_Node : Node_Type := Peek (Nodes);
      This_Partner : Partner_Access;
   begin
      Next (Nodes);

      Check (Partner_Checker, Partner_Node);

      This_Partner := Create (ID         => To_ID (Partner_Node ("label")),
                              Name       => Partner_Node ("name"),
                              Short_Name => Partner_Node ("short"),
                              Country    => Partner_Node ("country"),
                              Node_Dir   => Project.Node_Directory);

      while Class_Is (Nodes, Role) loop
         Parse_Role (Nodes, This_Partner);
      end loop;

      Project.Add_Partner (This_Partner);
   end Parse_Partner;

   procedure Parse_Metadata  (Nodes   : in out Node_Parser.Node_Scanner;
                              Project :    out EU_Projects.Projects.Project_Descriptor)
     with Pre => Class_Is (Nodes, Metadata);

   procedure Parse_Metadata  (Nodes   : in out Node_Parser.Node_Scanner;
                              Project :    out EU_Projects.Projects.Project_Descriptor)
   is
      use EU_Projects.Nodes.Timed_Nodes.Project_Infos;

      This_Node     : Node_Type := Peek (Nodes);
      This_Metadata : Info_Access;
   begin
      Next (Nodes);

      Check (Project_Checker, This_Node);

      This_Metadata := Project_Infos.Create (Label      => EU_Projects.Nodes.To_ID (This_Node ("label")),
                                             Name       => This_Node ("name"),
                                             Short_Name => This_Node ("short"),
                                             Stop_At    => This_Node ("end"),
                                             Node_Dir   => Project.Node_Directory);

      Project.Bless (This_Metadata);
   end Parse_Metadata;

   procedure Parse_Milestone (Nodes   : in out Node_Parser.Node_Scanner;
                              Project :    out EU_Projects.Projects.Project_Descriptor)
     with Pre => Class_Is (Nodes, Milestone);

   ---------------------
   -- Parse_Milestone --
   ---------------------

   procedure Parse_Milestone (Nodes   : in out Node_Parser.Node_Scanner;
                              Project :    out EU_Projects.Projects.Project_Descriptor)
   is
      use EU_Projects.Nodes.Timed_Nodes.Milestones;

      Milestone_Node : Node_Type := Peek (Nodes);
      This_Milestone : Milestone_Access;
   begin
      Next (Nodes);
      Check (Milestone_Checker, Milestone_Node);

      This_Milestone :=
        Create (Label        => Milestone_Label'(To_ID (Milestone_Node ("label"))),
                Name         => Milestone_Node ("name"),
                Short_Name   => Milestone_Node ("short"),
                Description  => Milestone_Node.Description,
                Due_On       => Milestone_Node ("when"),
                Node_Dir     => Project.Node_Directory,
                Verification => Milestone_Node ("verification"));

      Project.Add_Milestone (This_Milestone);
   end Parse_Milestone;

   ---------------------
   -- Parse_Top_Level --
   ---------------------

   procedure Parse_Top_Level (Nodes   : in out Node_Parser.Node_Scanner;
                              Project :    out EU_Projects.Projects.Project_Descriptor)
   is
   begin
      if not (Node_Parser.Peek_Class (Nodes) in Top_Level_Node) then
         raise Parsing_Error with "top level node expected";
      end if;

      case Top_Level_Node (Peek_Class (Nodes)) is
         when Metadata =>
            Parse_Metadata (Nodes, Project);

         when Wp =>
            Parse_Wp (Nodes, Project);

         when Partner =>
            Parse_Partner (Nodes, Project);

         when Milestone =>
            Parse_Milestone (Nodes, Project);
            --
            --           when Deliverable =>
            --              Parse_Deliverable (Nodes, Project);
      end case;
   end Parse_Top_Level;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Parser  : in out Parser_Type;
      Project :    out EU_Projects.Projects.Project_Descriptor;
      Input   : String)
   is
      pragma Unreferenced (Parser);

      Names : Name_Maps.Map;
   begin
      Names.Insert (Key      => +"task",
                    New_Item => Tsk);

      declare
         Nodes : Node_Scanner :=
                   To_Scanner (Parse (Input => Line_Arrays.Split (Input),
                                      Names => Names));
      begin
         while not End_Of_List (Nodes) loop
            Parse_Top_Level (Nodes, Project);
         end loop;
      end;
   end Parse;
begin
   Parser_Tables.Register (ID  => "simple",
                           Tag => Parser_Type'Tag);

   Register_Extension (Parser         => "simple",
                       File_Extension => "simple");

end Project_Processor.Parsers.Simple_Format;
