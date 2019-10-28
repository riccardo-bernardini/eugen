with XML_Scanners; use XML_Scanners;
with DOM.Core;
with XML_Utilities;
with Project_Processor.Parsers.XML_Parsers.Basic_Parsers;
with EU_Projects.Efforts;

with Ada.Containers;
with EU_Projects.Times.Time_Expressions;

package body Project_Processor.Parsers.XML_Parsers.Sub_Parsers is

   -------------------------
   -- Parse_Configuration --
   -------------------------

   procedure Parse_Configuration (Project : in out Project_Descriptor;
                                  Input   : in     DOM.Core.Node)
   is
      Scanner : XML_Scanner := Create_Child_Scanner (Input);

      procedure Handle_Parameter (N : Dom.Core.Node) is
         use XML_Utilities;

         Name  : constant String := Expect_Attribute (N, "name");
         Value : constant String := Expect_Attribute (N, "value");
      begin
         Project.Configure (Name, Value);
      end Handle_Parameter;
   begin
      Scanner.Parse_Sequence ("parameter", Handle_Parameter'Access);
   end Parse_Configuration;


   -------------------
   -- Parse_Partner --
   -------------------

   function Parse_Partner (N     : DoM.Core.Node)
                           return  EU_Projects.Nodes.Partners.Partner_Access
   is
      use XML_Utilities;
      use EU_Projects.Nodes.Partners;
      use Basic_Parsers;

      Label      : constant Partner_Label := Partner_Label (Expect_ID (N, "label"));
      Name       : constant String := Expect_Attribute (N, "name");
      Short_Name : constant String := Get_Attribute (N, "short", Name);

      Partner_Descriptor : constant Partner_Access := Create (ID         => Label,
                                                              Name       => Name,
                                                              Short_Name => Short_Name);

      Scanner : XML_Scanner := Create_Child_Scanner (N);

      procedure Parse_Budget (Budget_Node : DOM.Core.Node) is
         Scanner : XML_Scanner := Create_Child_Scanner (Budget_Node);

         procedure Parse_Personnel (Personnel_Node : DOM.Core.Node)
         is
            --              use Projects.Costs;

            Role          : constant Role_Name := Role_Name (Expect_ID (Personnel_Node, "role"));
            Unit_Cost     : constant Currency := Currency (Expect_Number (Personnel_Node, "month-cost"));
         begin
            Partner_Descriptor.Add_Role (Role         => Role,
                                         Description  => "",
                                         Monthly_Cost => Unit_Cost);
         end Parse_Personnel;

         procedure Parse_Other_Costs (Personnel_Node : DOM.Core.Node)
         is
            --              use Projects.Costs;

            Description : constant String   := Expect_Attribute (Personnel_Node, "description");
            Unit_Cost   : constant Currency := Currency (Expect_Number (Personnel_Node, "unit-cost"));
            Amount      : constant Natural  := Expect_Number (Personnel_Node, "amount");
         begin
            Partner_Descriptor.Add_Divisible_Expense
                  (Description => Description,
                   Unit_Cost   => Unit_Cost,
                   Amount      => Amount);
         end Parse_Other_Costs;
      begin
         Scanner.Parse_Sequence ("personnel", Parse_Personnel'Access);

         Scanner.Parse_Sequence (Name       => "other-cost",
                                 Callback   => Parse_Other_Costs'Access,
                                 Min_Length => 0);
      end Parse_Budget;
   begin
      Scanner.Parse_Single_Node ("budget", Parse_Budget'Access);

      return Partner_Descriptor;
   end Parse_Partner;

   ---------------------
   -- Parse_Milestone --
   ---------------------

   procedure Parse_Milestone
         (N         : in     DoM.Core.Node;
          Milestone :    out Nodes.Timed_Nodes.Milestones.Milestone_Access)
   is
      use XML_Utilities;
      use Basic_Parsers;
      use EU_Projects.Nodes.Timed_Nodes.Milestones;

      Label      : constant Milestone_Label := Milestone_Label (Expect_ID (N));
      Name       : constant String := Expect_Attribute (N, "name");
      Short_Name : constant String := Get_Attribute (N, "short", Name);
      Happen_On  : constant Times.Time_Expressions.Symbolic_Instant := Expect_Time (N, "when");

      Scanner : constant XML_Scanner := Create_Child_Scanner (N);

      Description : constant String := Scanner.Parse_Text_Node ("description");
   begin
      Scanner.Expect_End_Of_Children;

      Milestone := Create (Label       => Label,
                           Name        => Name,
                           Short_Name  => Short_Name,
                           Description => Description,
                           Due_On      => Happen_On);
   end Parse_Milestone;

   -----------------------
   -- Parse_Deliverable --
   -----------------------

   procedure Parse_Deliverable
         (Deliv_Node  : in     DOM.Core.Node;
          Deliv       :    out Nodes.Timed_Nodes.Deliverables.Deliverable_Access)
   is
      use EU_Projects.Nodes.Timed_Nodes.Deliverables;
      use EU_Projects.Nodes.Timed_Nodes;

      use type Ada.Containers.Count_Type;
      use XML_Utilities;
      use Basic_Parsers;

      Label          : constant Deliverable_Label := Deliverable_Label (Expect_ID (Deliv_Node));
      Name           : constant String := Expect_Attribute (Deliv_Node, "name");
      Tasks_Involved : constant ID_List := To_ID_List (Get_Attribute (Deliv_Node, "tasks", ""));
      Due_Date       : Times.Time_Expressions.Symbolic_Instant;
      Scanner        : constant XML_Scanner := Create_Child_Scanner (Deliv_Node);

      Delivered_By   : constant Node_Label_Lists.Vector :=
                         Parse_Label_List (Expect_Attribute (Deliv_Node, "delivered-by"));

      Description : constant String := Scanner.Parse_Text_Node ("description");
   begin
      if Has_Attribute (Deliv_Node, "due-date") then
         Due_Date := Expect_Time (Deliv_Node, "due-date");
      elsif Tasks_Involved.Length = 1 then
         Due_Date := Times.Time_Expressions.Parse (Image (Tasks_Involved.First_Element) & ".end");
      else
         raise Parsing_Error;
      end if;

      Deliv := Deliverables.Create (Label        => Label,
                                    Name         => Name,
                                    Description  => Description,
                                    Delivered_By => Delivered_By,
                                    Due_On       => Due_Date);

      --        Equations.Insert (Key      => Event_Names.Deliverable_Var (Parent_Task, Identifiers.Identifier (Label)),
      --                          New_Item => Due_Date);
   end Parse_Deliverable;

   ----------------
   -- Parse_Task --
   ----------------

   procedure Parse_Task
         (Task_Node    : in     DOM.Core.Node;
          Parent_WP    : in     Nodes.Action_Nodes.WPs.Project_WP_Access;
          Descriptor   :    out Nodes.Action_Nodes.Tasks.Project_Task_Access)
   is
      use EU_Projects.Nodes.Action_Nodes.Tasks;
      use XML_Utilities;
      use Basic_Parsers;
      use EU_Projects.Nodes.Partners;
      --        use Event_Names;

      Label      : constant Task_Label := Task_Label (Expect_ID (Task_Node));
      Name       : constant String := Expect_Attribute (Task_Node, "name");
      Short_Name : constant String := Get_Attribute (Task_Node, "short", Name);
      Leader     : constant Partner_Label := Partner_Label (Expect_ID (Task_Node, "leader"));
      Start_On   : constant Times.Time_Expressions.Symbolic_Instant := Expect_Time (Task_Node, "start");
      End_On     : Times.Time_Expressions.Symbolic_Instant;
      Duration   : Times.Time_Expressions.Symbolic_Duration;

      Scanner : XML_Scanner := Create_Child_Scanner (Task_Node);

      Description : constant String := Scanner.Parse_Text_Node ("description");

      Dependences : EU_Projects.Nodes.Node_Label_Lists.Vector;

      procedure Add_Effort (Effort_Node : DOM.Core.Node) is
         use Efforts;

         Partner : constant Partner_Label := Partner_Label (Expect_ID (Effort_Node, "partner"));
         PM      : constant Person_Months := Person_Months (Expect_Number (Effort_Node, "person-months"));
      begin
         Descriptor.Add_Effort (Partner, PM);
      exception
         when EU_Projects.Nodes.Action_Nodes.Tasks.Duplicated_Effort =>
            raise Parsing_Error;
      end Add_Effort;

      procedure Add_Dependence (Depend_Node : DOM.Core.Node) is
      begin
         Dependences.Append (EU_Projects.Nodes.Node_Label (Expect_ID (Depend_Node)));
      end Add_Dependence;
   begin
      Get_End_And_Duration (N          => Task_Node,
                            Start_Time => Start_On,
                            End_Time   => End_On,
                            Duration   => Duration);

      --        Equations.Insert (Key      => Task_Var (Parent_WP, Label, Start_Time),
      --                          New_Item => Start_On);
      --
      --        Equations.Insert (Key      => Task_Var (Parent_WP, Label, End_Time),
      --                          New_Item => End_On);
      --
      --        Equations.Insert (Key      => Task_Var (Parent_WP, Label, Duration_Time),
      --                          New_Item => Duration);


      Scanner.Parse_Sequence ("effort", Add_Effort'Access);

      Scanner.Parse_Sequence (Name       => "depend-on",
                              Callback   => Add_Dependence'Access,
                              Min_Length => 0);

      Descriptor := Create (Label       => Label,
                            Name        => Name,
                            Short_Name  => Short_Name,
                            Leader      => Leader,
                            Parent_WP   => EU_Projects.Nodes.Node_Access (Parent_WP),
                            Description => Description,
                            Action_Time => Nodes.Action_Nodes.Create (Start_On, End_On),
                            Depends_On  => Dependences);

   end Parse_Task;

   --------------
   -- Parse_WP --
   --------------

   procedure Parse_WP
         (WP_Node   : in     DOM.Core.Node;
          WP        :    out Nodes.Action_Nodes.WPs.Project_WP_Access)
   is
      use XML_Utilities;
      use Basic_Parsers;
      use EU_Projects.Nodes.Action_Nodes.WPs;

      Label      : constant WP_Label := WP_Label (Expect_ID (WP_Node));
      Name       : constant String := Expect_Attribute (WP_Node, "name");
      Short_Name : constant String := Get_Attribute (WP_Node, "short", Name);
      Leader     : constant EU_Projects.Nodes.Partners.Partner_Label := EU_Projects.Nodes.Partners.Partner_Label (Expect_ID (WP_Node, "leader"));
      Start_On   : constant Times.Time_Expressions.Symbolic_Instant := Expect_Time (WP_Node, "start");
      End_On     : Times.Time_Expressions.Symbolic_Instant;
      Duration   : Times.Time_Expressions.Symbolic_Duration;




      Scanner     : XML_Scanner := Create_Child_Scanner (WP_Node);
      Objectives  : constant String := Scanner.Parse_Text_Node ("objectives");
      Description : constant String := Scanner.Parse_Text_Node ("description");
      Dependences : Node_Label_Lists.Vector;

      procedure Add_Dependence (Depend_Node : DOM.Core.Node) is
      begin
         Dependences.Append (Node_Label (Expect_ID (Depend_Node)));
      end Add_Dependence;

      procedure Add_Task (N : DOM.Core.Node) is

         Descriptor : Nodes.Action_Nodes.Tasks.Project_Task_Access;
      begin
         Parse_Task (Task_Node  => N,
                     Parent_WP  => WP,
                     Descriptor => Descriptor);

         WP.Add_Task (Descriptor);
      end Add_Task;


   begin
      Get_End_And_Duration (N          => WP_Node,
                            Start_Time => Start_On,
                            End_Time   => End_On,
                            Duration   => Duration);

      Scanner.Parse_Sequence (Name       => "depend-on",
                              Callback   => Add_Dependence'Access,
                              Min_Length => 0);

      WP := Action_Nodes.WPs.Create (Label       => Label,
                                     Name        => Name,
                                     Short_Name  => Short_Name,
                                     Leader      => Leader,
                                     Objectives  => Objectives,
                                     Description => Description,
                                     Action_Time => Action_Nodes.Create (Start_On => Start_On,
                                                                         End_On   => End_On),
                                     Depends_On  => Dependences);


      Scanner.Parse_Sequence ("task", Add_Task'Access);



      --        WP.Summarize_Tasks;
   end Parse_WP;

   function Parse_Risk (Risk_Node : DOM.Core.Node)
                        return Nodes.Risks.Risk_Access
   is
      use XML_Utilities;
      use Basic_Parsers;
      use Risks;

      function To_Likeness (X : String) return Risk_Likeness
      is
      begin
         if X = "small" then
            return Small;
         elsif X = "medium" then
            return Medium;
         elsif X = "large" then
            return Large;
         else
            raise Parsing_Error;
         end if;
      end To_Likeness;

      function To_Severity (X : String) return Risk_Severity
      is
      begin
         if X = "small" then
            return Small;
         elsif X = "medium" then
            return Medium;
         elsif X = "large" then
            return Large;
         else
            raise Parsing_Error;
         end if;
      end To_Severity;

      Label    : constant Risk_Label := Risk_Label (Expect_ID (Risk_Node));
      Severity : constant Risk_Severity := To_Severity (Expect_Attribute (Risk_Node, "severity"));
      Likeness : constant Risk_Likeness := To_Likeness (Expect_Attribute (Risk_Node, "likeness"));

      Scanner : constant XML_Scanner := Create_Child_Scanner (Risk_Node);

      Description     : constant String := Scanner.Parse_Text_Node ("description");
      Countermeasures : constant String := Scanner.Parse_Text_Node ("countermeasures");
   begin
      return New_Risk (Label           => Label,
                       Description     => Description,
                       Countermeasures => Countermeasures,
                       Severity        => Severity,
                       Likeness        => Likeness);
   end Parse_Risk;

end Project_Processor.Parsers.XML_Parsers.Sub_Parsers;
