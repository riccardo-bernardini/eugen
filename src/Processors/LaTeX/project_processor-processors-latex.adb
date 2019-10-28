pragma Ada_2012;
with Ada.Strings.Unbounded;
with Ada.Strings.Equal_Case_Insensitive;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with EU_Projects.Nodes.Partners;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Action_Nodes.Tasks;

with Project_Processor.Processors.Processor_Tables;
with EU_Projects.Times;

with Latex_Writer.Picture;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;
with EU_Projects.Nodes.Timed_Nodes.Milestones;
with EU_Projects.Efforts;

package body Project_Processor.Processors.LaTeX is
   use EU_Projects.Projects;
   use EU_Projects.Nodes;
   use EU_Projects.Nodes.Action_Nodes;
   use EU_Projects.Nodes.Timed_Nodes;
   use Latex_Writer.Picture;
   use EU_Projects.Times;

   use Ada.Strings.Unbounded;

   Formatter : constant Time_Formatter :=
                 Create.Set_Option (To_Be_Decided_Image, "\TBD");

   type Table_Style is (Light, Full);

   subtype Deliverable_Type_Image_Value is String (1 .. 5);

   Deliverable_Type_Image                                                : constant
         array (Deliverables.Deliverable_Type) of Deliverable_Type_Image_Value :=
                                                                             (Deliverables.Report        => "R    ",
                                                                              Deliverables.Demo          => "DEM  ",
                                                                              Deliverables.Dissemination => "DEC  ",
                                                                              Deliverables.Other         => "OTHER");

   subtype Dissemination_Level_Image_Value is String (1 .. 2);

   Dissemination_Level_Image : constant array (Deliverables.Dissemination_Level)
         of Dissemination_Level_Image_Value :=
               (Deliverables.Public       => "PU",
                Deliverables.Confidential => "CO",
                Deliverables.Classified   => "Cl");

   -- The result of function 'Image associated to discrete types has
   -- a space at the beginning.  That space is quite annoying and needs
   -- to be trimmed.  This function is here so that everyone can use it

   function Chop (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Image (X : Integer) return String
   is (Chop (Integer'Image (X)));


   type Tone is (Light, Medium, Dark);

   function Gray (T : Natural) return String
   is ("\cellcolor[gray]{0." & Image (T) & "}");

   function Gray (T : Tone := Medium) return String
   is (Gray ((case T is
                 when Light  => 95,
                 when Medium => 90,
                 when Dark   => 80)));
   ------------
   -- Create --
   ------------

   overriding function Create
         (Params : not null access Processor_Parameter) return Processor_Type
   is
      function Get_Target (Key : String) return Target_Spec
      is
      begin
         if not Params.Contains (Key) then
            return Target_Spec'(Class    => File,
                                Filename => Bless (Key & "s.tex"));

         elsif Params.all (Key) = "%" then
            return Target_Spec'(Class => None);

         elsif Params.all (Key) = "-" then
            return Target_Spec'(Class => Standard_Output);

         else
            return Target_Spec'(Class    => File,
                                Filename => Bless (Params.all (Key)));
         end if;
      end Get_Target;

      -------------------------
      -- Extract_Gantt_Style --
      -------------------------

      function Extract_Gantt_Style return GANTT_Parameters
      is
         Result : GANTT_Parameters :=
                    GANTT_Parameters'(Font_Size         => 11.0 * Pt,
                                      Textwidth         => 160.0 * Mm,
                                      Pre_Label_Skip    => 0.125,
                                      Post_Label_Skip   => 0.125,
                                      Header_Skip       => 0.2,
                                      Tick_Length       => 0.5,
                                      Task_Indent       => 1,
                                      Show_Deliverables => False);

         function Eq (X, Y : String) return Boolean
                      renames Ada.Strings.Equal_Case_Insensitive;

         function To_Boolean (X : String) return Boolean
         is (if Eq (X, "") or Eq (X, "y") or Eq (X, "t") or Eq (X, "yes") then
                True
             elsif Eq (X, "n") or Eq (X, "f") or Eq (X, "no") then
                False
             else
                raise Constraint_Error);
      begin
         if Params.Contains ("show-deliverables") then
            Result.Show_Deliverables := To_Boolean (Params.all ("show-deliverables"));
         end if;

         return Result;
      end Extract_Gantt_Style;


   begin
      return Processor_Type'(Wp_Target             => Get_Target ("wp"),
                             Partner_Target        => Get_Target ("partner"),
                             Deliverable_Target    => Get_Target ("deliverable"),
                             Wp_Summary_Target     => Get_Target ("summary-wp"),
                             Milestone_Target      => Get_Target ("milestone"),
                             Deliv_Summary_Target  => Get_Target ("summary-deliv"),
                             Deliv_Compact_Summary_Target  => Get_Target ("compact-summary-deliv"),
                             Gantt_Target          => Get_Target ("gantt"),
                             Effort_Summary_Target => Get_Target ("summary-effort"),
                             Gantt_Style           => Extract_Gantt_Style);
   end Create;

   type File_Pt is access File_Type;

   type Extended_File (Class : Target_Class := File) is
      record
         case Class is
         when None | Standard_Output =>
            null;

         when File =>
            F : File_Pt;
         end case;
      end record;

   function Open (What : Target_Spec) return Extended_File
   is
   begin
      case What.Class is
         when None =>
            return Extended_File'(Class => None);

         when Standard_Output =>
            return Extended_File'(Class => Standard_Output);

         when File =>
            declare
               Result : constant File_Pt := new File_Type;
            begin
               Create (File => Result.all,
                       Mode => Out_File,
                       Name => To_S (What.Filename));

               return Extended_File'(Class => File,
                                     F     => Result);
            end;
      end case;
   end Open;

   procedure Close (What : in out Extended_File)
   is
      procedure Free is
            new Ada.Unchecked_Deallocation (Object => File_Type,
                                            Name   => File_Pt);
   begin
      case What.Class is
         when None | Standard_Output =>
            null;

         when File =>
            Close (What.F.all);
            Free (What.F);
      end case;

   end Close;

   function To_File_Access (What : Extended_File) return File_Access
   is (case What.Class is
          when None            =>
             null,

          when Standard_Output =>
             Standard_Output,

          when File            =>
             File_Access (What.F));


   --------------------------
   -- Print_Warning_Header --
   --------------------------

   procedure Print_Warning_Header (To : File_Access)
   is

   begin
      Put_Line (To.all, "%");
      Put_Line (To.all, "%---");
      Put_Line (To.all, "%  WARNING: Automatically generated file");
      Put_Line (To.all, "%  WARNING: If you edit this your changes will be lost");
      Put_Line (To.all, "%---");
      Put_Line (To.all, "%");
      New_Page (To.all);
   end Print_Warning_Header;


   --------------------
   -- Print_Partners --
   --------------------

   procedure Print_Partners (Input  : EU_Projects.Projects.Project_Descriptor;
                             Target : Target_Spec)
   is
      use EU_Projects.Nodes.Partners;

      procedure Print_Partners (To : File_Access) is
         procedure Print_Single_Partner (To : File_Type; Partner : Partner_Access) is
         begin
            Put_Line (To, "\newpartner"
                      & "{" & Partner.Short_Name & "}"
                      & "{" & To_String (Partner.Label) & "}"
                      & "{" & Partner.Name & "}"
                      & "{" & Partner.Country & "}");
         end Print_Single_Partner;
      begin
         for Idx in Input.All_Partners loop
            Print_Single_Partner (To.all, Element (Idx));
         end loop;
      end Print_Partners;

      Output : Extended_File := Open (Target);
   begin
      if Output.Class = None then
         return;
      end if;

      Within (Output    => To_File_Access (Output),
              Env_Name  => "partnerlist",
              Callback  => Print_Partners'Access);

      Close (Output);
   end Print_Partners;


   ------------------
   -- Define_Label --
   ------------------

   procedure Define_Label (Output       : File_Type;
                           Item         : Node_Type'Class;
                           Prefix       : String;
                           Add_New_Line : Boolean := True)
   is
   begin
      Put (Output,
           "\failabel{" & Image (Item.Label) & "}"
           & "{" & Prefix & "}"
           & "{" & Item.Short_Name & "}"
           & "{" & Item.Full_Index (False) & "}");

      if Add_New_Line then
         New_Line (Output);
      end if;
   end Define_Label;
   ------------------
   -- Join_Indexes --
   ------------------

   function Join_Indexes (Input     : EU_Projects.Projects.Project_Descriptor;
                          Labels    : Node_Label_Lists.Vector;
                          Separator : String)
                          return String
   is
      Result : Unbounded_String;
   begin
      for Idx in Labels.Iterate loop
         Result := Result & Input.Find (Labels (Idx)).Full_Index (Prefixed => True);

         if Node_Label_Lists.To_Index (Idx) < Labels.Last_Index then
            Result := Result & Separator;
         end if;
      end loop;

      return To_String (Result);
   end Join_Indexes;

   procedure Print_WP (Input  : Project_Descriptor;
                       Output : Extended_File;
                       WP     : WPs.Project_WP_Access) is


      Efforts : constant Action_Nodes.Effort_List :=
                  WP.Efforts_Of (Input.Partner_Names);


      function Short_Name (X : Partners.Partner_Label) return String
      is
      begin
         return Input.Find (Node_Label (X)).Short_Name;
      end Short_Name;

      procedure Write_WP_Header (Output : File_Access;
                                 Table  : in out Table_Handler)
      is
         pragma Unreferenced (Output);

         Headstyle : constant String := "\stilehead";

         procedure Put_Pair (Title, Content : String) is
         begin
            Table.Put (Title, Headstyle);
            Table.Put (Content);
         end Put_Pair;

         procedure First_Row_Standard_Style is
         begin
            Table.Cline (1, 4);
            Put_Pair ("WP Number", WP.Index_Image);
            Put_Pair ("Leader", Short_Name (WP.Leader));
            Table.Hline;

            Table.Put ("WP Name", Headstyle);
            Table.Multicolumn (Span    => Efforts'Length + 1,
                               Spec    => "|l|",
                               Content => WP.Name);

         end First_Row_Standard_Style;

         procedure First_Row_Compact_Style is
         begin
            Table.Hline;
            Put_Pair ("WP N.", WP.Index_Image);

            Table.Put ("WP Name", Headstyle);
            Table.Multicolumn (Span    => Efforts'Length - 4,
                               Spec    => "|l|",
                               Content => WP.Name);

            Table.Put ("\WPleadertitle");
            Table.Multicolumn (Span    => 2,
                               Spec    => "c|",
                               Content =>
                                 "\WPleadername{" & Short_Name (WP.Leader) & "}");
         end First_Row_Compact_Style;
      begin
         if True then
            First_Row_Compact_Style;
         else
            First_Row_Standard_Style;
         end if;

         Table.Hline;

         Table.Put ("N. Partner", Headstyle);

         for Idx in Efforts'Range loop
            Table.Put (Image (Idx));
         end loop;
         Table.Put ("");
         Table.Hline;

         Table.Put ("Name", Headstyle);

         for Idx in Efforts'Range loop
            Table.Put (Short_Name (Efforts (Idx).Partner));
         end loop;
         Table.Put ("all");
         Table.Hline;

         Table.Put ("PM", Headstyle);

         declare
            use EU_Projects.Efforts;

            Total_Effort : Person_Months := 0;
         begin
            for Idx in Efforts'Range loop
               Table.Put (Chop (Efforts (Idx).Effort'Image));

               Total_Effort := Total_Effort + Efforts (Idx).Effort;
            end loop;

            Table.Put (Chop (Total_Effort'Image));
         end;
         Table.Hline;

         Put_Pair ("Start", "M" & EU_Projects.Times.Image (Formatter, WP.Starting_Time));
         Put_Pair ("End", "M" & EU_Projects.Times.Image (Formatter, WP.Ending_Time));
         Table.Cline (1, 4);
      end Write_WP_Header;

      -------------------------
      -- Write_WP_objectives --
      -------------------------

      procedure Write_WP_Objectives (Output : File_Access)
      is
      begin
         if WP.Description /= "" then
            Put_Line (Output.all, Wp.Description);
         else
            Put_Line (Output.all, "Placeholder, to be written");
         end if;
      end Write_WP_Objectives;

      --------------------------
      -- Write_WP_Description --
      --------------------------

      procedure Write_WP_Description (Output : File_Access)
      is
         procedure Write_WP_Tasks  (Output : File_Access)
         is
            procedure Print_Task (Output : File_Access;
                                  Tsk    : Tasks.Project_Task_Access) is
            begin
               Put_Line (Output.all,
                         "\HXXitem{" &  Tsk.Full_Index (Prefixed => True) & "}"
                         & "{" & Tsk.Name & "}"
                         & "{("
                         & Tsk.Timing & "; Leader: " & Short_Name (Tsk.Leader)
                         & ")}");


               Define_Label (Output => Output.all,
                             Item   => Tsk.all,
                             Prefix => "T");

               Put_Line (Output.all, Tsk.Description);
            end Print_Task;
         begin
            if Wp.Max_Task_Index > 0 then
               for Idx in WP.All_Tasks loop
                  Print_Task (Output, WPs.Element (Idx));
               end loop;
            else
               Put_Line (Output.all, "\HXXitem{T0.0}{Placeholder}{(please ignore)}");
            end if;
         end Write_WP_Tasks;
      begin
         Within (Output    => Output,
                 Env_Name  => "HXXitemize",
                 Callback  => Write_WP_Tasks'Access);
      end Write_WP_Description;

      ---------------------------
      -- Write_WP_Deliverables --
      ---------------------------

      procedure Write_WP_Deliverables (Output : File_Access)
      is
         procedure Loop_Over_Deliv (Output : File_Access)
         is
            procedure Print_Deliv (Output : File_Access;
                                   Deliv  : Deliverables.Deliverable_Access)
            is
               use type Deliverables.Deliverable_Status;

               N_Deliverers : constant Natural := Natural (Deliv.Delivered_By.Length);

               function Add_Colon_If_Not_Empty (X : String) return String
               is (if X = ""  then "" else ": " & X);
            begin
               if Deliv.Status = Deliverables.Clone then
                  return;
               end if;

               Put (Output.all,
                    "\wpdeliv{" &  Deliv.Full_Index (Prefixed => True) & "}"
                    & "{" & Deliv.Name & "}"
                    & "{" & Add_Colon_If_Not_Empty (Deliv.Description) & "}"
                    & "{("
                    & "Due: M" & Deliverables.Image (Deliv.Due_On)
                    --                           & "; Nature: " & "WRITE ME"
                    & (case N_Deliverers is
                             when 0      => "",
                             when 1      => " Task:",
                             when others => " Tasks:")
                    & Join_Indexes (Input, Deliv.Delivered_By, ", ")
                    & ")}");

               Define_Label (Output       => Output.all,
                             Item         => Deliv.all,
                             Prefix       => "D",
                             Add_New_Line => False);

               --                 Put_Line (Output.all, Deliv.Description);
            end Print_Deliv;

            At_Least_One_Deliverable : Boolean := False;
         begin
            for Idx in WP.All_Deliverables loop
               Print_Deliv (Output, WPs.Element (Idx));
               At_Least_One_Deliverable := True;
            end loop;

            if not At_Least_One_Deliverable then
               Put_Line (Output.all, "\wpdeliv{D0.0}{Placeholder}{(please ignore)}");
            end if;
         end Loop_Over_Deliv;
      begin
         Within (Output    => Output,
                 Env_Name  => "wpdeliverables",
                 Callback  => Loop_Over_Deliv'Access);
      end Write_WP_Deliverables;



   begin
      if Output.Class = None then
         return;
      end if;

      Define_Label (To_File_Access (Output).all, WP.all, "WP");

      if WP.Index > 1 then
         Put_Line (To_File_Access (Output).all, "\beforeheaderskip");
      end if;

      Put_Line (To_File_Access (Output).all, "\noindent{\headersize");

      Within_Table (Output        => To_File_Access (Output),
                    Table_Spec    => "|l*{" & Project_Processor.Image (Efforts'Length + 1)  & "}{|c}|",
                    Callback      => Write_WP_Header'Access,
                    Default_Style => "\stilecontent");

      Put_Line (To_File_Access (Output).all, "}\\[\wpheadersep]");

      Within (Output    => To_File_Access (Output),
              Env_Name  => "titledbox",
              Callback  => Write_Wp_Objectives'Access,
              Parameter => "Objectives");

      Within (Output    => To_File_Access (Output),
              Env_Name  => "titledbox",
              Callback  => Write_WP_Description'Access,
              Parameter => "Description");

      Within (Output    => To_File_Access (Output),
              Env_Name  => "titledbox",
              Callback  => Write_WP_Deliverables'Access,
              Parameter => "Deliverables");


   end Print_WP;

   procedure Print_WPs (Input          : EU_Projects.Projects.Project_Descriptor;
                        Output         : Target_Spec)
   is
      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;


      Print_Warning_Header (To_File_Access (Target));
      Print_Default_Macro (To_File_Access (Target), "\stilecontent", "#1", 1);
      Print_Default_Macro (To_File_Access (Target), "\stilehead", "\textbf{#1}", 1);
      Print_Default_Macro (To_File_Access (Target), "\headersize", "\footnotesize", 0);

      for Idx in Input.All_WPs loop
         Print_WP (Input, Target, Element (Idx));
      end loop;

      Close (Target);
   end Print_WPs;

   procedure Print_Milestones (Input  : EU_Projects.Projects.Project_Descriptor;
                               Output : Target_Spec;
                               Style  : Table_Style)
   is
      pragma Unreferenced (Style);
      use EU_Projects.Nodes.Timed_Nodes.Milestones;

      procedure Loop_Over_Milestones (Output : File_Access)
      is
         procedure Print_Milestone (Mstone : Milestone_Access)
         is
            function Image (X         : Node_Label_Lists.Vector;
                            Separator : String) return String
            is
               Result : Unbounded_String;
               Deliv  : Deliverables.Deliverable_Access;
               N      : Node_Access;
            begin
               for Lb of X loop
                  if Result /= Null_Unbounded_String then
                     Result := Result & Separator;
                  end if;

                  N := Input.Find (Lb);
                  if N = null then
                     Put_Line (Standard_Error, "Deliverable '" & To_String (Lb) & "' unknown");
                  else
                     Deliv := Deliverables.Deliverable_Access (N);


                     Result := Result & "\ref{" & To_String (Deliv.Parent_Wp.Label) & "}";
                  end if;
               end loop;

               return To_String (Result);
            end Image;


         begin
            Put_Line (Output.all, "\milestoneitem" &
                            "[" & Mstone.Description & "]" &
                            "{" & Mstone.Full_Index (Prefixed => False) & "}" &
                            "{" & Mstone.Name & "}" &
                            "{M" & EU_Projects.Times.Image (Formatter, Mstone.Due_On) & "}" &
                            "{" & Image (Mstone.Deliverable_List, ", ")  & "}" &
                            "{" & Mstone.Verification_Mean & "}" &
                            "{" & Image (Mstone.Label) & "}" &
                            "{" & Mstone.Short_Name & "}");


            --              Table.Put (Mstone.Full_Index (Prefixed => True));
            --              Table.Put (Mstone.Name);
            --              Table.Put (Join_Indexes (Input, Mstone.Deliverable_List, ", "));
            --              Table.Put ("M" & EU_Projects.Times.Image (Mstone.Due_On));
            --              Table.Put (Mstone.Verification_Mean);
            --
            --              if Style = Full then
            --                 Table.Hline;
            --
            --                 Table.Multicolumn (Span    => 5,
            --                                    Spec    => (case Style is
            --                                                   when Full  => "|l|",
            --                                                   when Light => "l"),
            --                                    Content => Mstone.Description);
            --              else
            --                 Table.Put (Mstone.Description);
            --              end if;
            --
            --
            --
            --              Define_Label (Output => Output.all,
            --                            Item   => Mstone.all,
            --                            Prefix => "M");
            --
            --              Table.Hline (Style = Full);
         end Print_Milestone;
      begin
         --           Table.Hline (Style = Full);
         --           Table.Multicolumn (1, "c|", "");
         --           Table.Multicolumn (1, "c|", "");
         --           Table.Multicolumn (1, "c|", "Deliverable");
         --           Table.Multicolumn (1, "c|", "");
         --           Table.Multicolumn (1, "c|", "Means of");
         --
         --           Table.New_Row;
         --
         --           Table.Multicolumn (1, "c|", "N.");
         --           Table.Multicolumn (1, "c|", "Name");
         --           Table.Multicolumn (1, "c|", "involved");
         --           Table.Multicolumn (1, "c|", "Date");
         --           Table.Multicolumn (1, "c|", "verification");
         --
         --           if Style = Light then
         --              Table.Multicolumn (1, "c", "Description");
         --           end if;
         --           Table.Hline;

         for Idx in Input.All_Milestones loop
            Print_Milestone (Element (Idx));
         end loop;
      end Loop_Over_Milestones;

      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;

      Print_Warning_Header (To_File_Access (Target));

      Within (Output    => To_File_Access (Target),
              Env_Name  => "milestonetable",
              Callback  => Loop_Over_Milestones'Access);

      --        Within_Table (Output        => To_File_Access (Target),
      --                      Table_Spec    => (case Style is
      --                                           when Full  => "|*5{l|}",
      --                                           when Light => "*5{l|}X"),
      --                      Callback      => Loop_Over_Milestones'Access,
      --                      Default_Style => "",
      --                      Caption       => "Milestones \label{tbl:milestones}");
      Close (Target);
   end Print_Milestones;

   ----------------------
   -- Print_WP_Summary --
   ----------------------

   procedure Print_WP_Summary (Input  : EU_Projects.Projects.Project_Descriptor;
                               Output : Target_Spec;
                               Style  : Table_Style)
   is
      pragma Unreferenced (Style);
      procedure Make_Summary (Output : File_Access;
                              Table  : in out Table_Handler)
      is
         pragma Unreferenced (Table);
         use EU_Projects.Efforts;
         use EU_Projects.Nodes.Action_Nodes.WPs;

         ------------------
         -- Total_Effort --
         ------------------

         function Total_Effort (WP : Project_WP_Access) return Person_Months
         is
            Result          : Person_Months := 0;
            Partner_Efforts : constant Effort_List := Wp.Efforts_Of (Input.Partner_Names);
         begin
            for Val of Partner_Efforts loop
               Result := Result + Val.Effort;
            end loop;

            return Result;
         end Total_Effort;

         --           procedure Full_Header is
         --
         --           begin
         --              Table.Hline;
         --              Table.Head ("WP");
         --              Table.Head ("WP Name");
         --              Table.Multicolumn (2, "|c|", "\stilehead{Leader}");
         --              --           Table.Put ("Leader N.", "\stilehead");
         --              --           Table.Put ("Leader"");
         --              Table.Head ("PM");
         --              Table.Head ("Start");
         --              Table.Head ("End");
         --              Table.Cline (3, 4);
         --
         --              Table.Put ("");
         --              Table.Put ("");
         --              Table.Head ("Name");
         --              Table.Head ("N.");
         --              Table.Put ("");
         --              Table.Put ("");
         --              Table.Put ("");
         --           end Full_Header;
         --
         --           procedure Light_Header is
         --
         --           begin
         --              Table.Put ("");
         --              Table.Put ("");
         --              Table.Multicolumn (2, "c", "\stilehead{Leader}");
         --              --           Table.Put ("Leader N.", "\stilehead");
         --              --           Table.Put ("Leader"");
         --              Table.Put ("");
         --              Table.Put ("");
         --              Table.Put ("");
         --              Table.Put ("");
         --              Table.Cline (3, 4);
         --
         --              Table.Multicolumn (1, "c", "WP");
         --              Table.Multicolumn (1, "c", "WP Name");
         --              Table.Multicolumn (1, "c", "Name");
         --              Table.Multicolumn (1, "c", "N.");
         --              Table.Multicolumn (1, "c", "~");
         --              Table.Multicolumn (1, "c", "PM");
         --              Table.Multicolumn (1, "c", "Start");
         --              Table.Multicolumn (1, "c", "End");
         --           end Light_Header;

         Project_Effort : Person_Months := 0;

      begin
         --           case Style is
         --              when Full => Full_Header;
         --              when Light => Light_Header;
         --           end case;

         Put_Line (Output.all, "\summarywptableheader");

         --           Table.Hline;


         for Pos in Input.All_WPs loop
            declare
               use EU_Projects.Nodes.Partners;

               procedure Put_Arg (X : String) is
               begin
                  Put (Output.all, "{" & X & "}");
               end Put_Arg;

               WP        : constant Project_WP_Access := Element (Pos);
               Wp_Effort : constant Person_Months := Total_Effort (Wp);
               Leader    : constant Partner_Access :=
                             Partner_Access (Input.Find (Node_Label (WP.Leader)));
            begin
               Put (Output.all, "\summarywpitem");

               Put_Arg (WP.Full_Index (Prefixed => False));
               Put_Arg (WP.Short_Name);
               Put_Arg (Leader.Short_Name);
               Put_Arg (Leader.Full_Index (Prefixed => False));
               Put_Arg (Chop (Wp_Effort'Image));
               Put_Arg (EU_Projects.Times.Image (Formatter, Wp.Starting_Time));
               Put_Arg (EU_Projects.Times.Image (Formatter, Wp.Ending_Time));

               New_Line (Output.all);
               Project_Effort := Project_Effort + Wp_Effort;
               --                 Table.Hline (Style = Full);
            end;
         end loop;

         Put_Line (Output.all, "\summarywptotalrow{" & Chop (Project_Effort'Image) & "}");

         --           case Style is
         --              when Full =>
         --                 Table.Put ("\cellavuota");
         --                 Table.Put ("\cellavuota");
         --                 Table.Put ("\cellavuota");
         --                 Table.Put ("\cellavuota");
         --                 Table.Put (Chop (Project_Effort'Image));
         --                 Table.Put ("\cellavuota");
         --                 Table.Put ("\cellavuota");
         --                 Table.Hline;
         --
         --              when Light =>
         --                 Table.Hline;
         --                 Put (Output.all, "\rigatotali");
         --                 Table.Put ("");
         --                 Table.Put ("Total");
         --                 Table.Put ("");
         --                 Table.Put ("");
         --                 Table.Put ("");
         --                 Table.Put (Chop (Project_Effort'Image));
         --                 Table.Put ("");
         --                 Table.Put ("");
         --           end case;
      end Make_Summary;

      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;

      Within_Table_Like (Output    => To_File_Access (Target),
                         Env_Name  => "summarywptable",
                         Callback  => Make_Summary'Access,
                         Parameter => "List of work packages \label{tbl:wps}");
      --        Within_Table (Output        => To_File_Access (Target),
      --                      Table_Spec    => (case Style is
      --                                           when Full  => "|c|X|c|c|r|r|r|",
      --                                           when Light => "cXlccrrr"),
      --                      Callback      => Make_Summary'Access,
      --                      Default_Style => "",
      --                      Default_Head  => "\textbf",
      --                      caption       => "List of work packages \label{tbl:wps}");

      Close (Target);
   end Print_WP_Summary;

   -------------------------
   -- Print_Deliv_Summary --
   -------------------------

   procedure Print_Deliv_Summary_Low_Level (Input  : EU_Projects.Projects.Project_Descriptor;
                                            Output : Target_Spec;
                                            Style  : Table_Style)
   is
      ------------------
      -- Make_Summary --
      ------------------

      procedure Make_Summary (Output : File_Access;
                              Table  : in out Table_Handler)
      is
         pragma Unreferenced (Output);
         use EU_Projects.Nodes.Timed_Nodes.Deliverables;
         --           use type EU_Projects.Times.Instant;

         function "<" (X, Y : Deliverable_Access) return Boolean
         is (EU_Projects.Times.Instant'(X.Due_On) < Y.Due_On
             or else
                   (EU_Projects.Times.Instant'(X.Due_On) = Y.Due_On
                    and X.Full_Index (False) < Y.Full_Index (False)));

         package Deliverable_Vectors is
               new Ada.Containers.Vectors (Index_Type   => Positive,
                                           Element_Type => Deliverable_Access);

         package Deliv_Sorting is
               new Deliverable_Vectors.Generic_Sorting;

         Project_Deliverables : Deliverable_Vectors.Vector;


      begin
         Table.Hline (Style = Full);
         Table.Put ("N.", "\stilehead");
         Table.Put ("Name", "\stilehead");
         Table.Put ("WP", "\stilehead");
         Table.Put ("Leader", "\stilehead");
         Table.Put ("Type", "\stilehead");
         Table.Put ("Policy", "\stilehead");
         Table.Put ("Due", "\stilehead");
         Table.Hline;

         for Pos in Input.All_Deliverables loop
            Project_Deliverables.Append (Element (Pos));
         end loop;

         Deliv_Sorting.Sort (Project_Deliverables);

         for Deliv of Project_Deliverables loop
            declare
               use Wps;
               use Partners;

               Parent : constant Project_WP_Access :=  Project_WP_Access (Deliv.Parent_Wp);
               Leader : constant Partner_Access :=
                          Partner_Access (Input.Find (Node_Label (Parent.Leader)));
            begin
               Table.Put (Deliv.Full_Index (Prefixed => True));
               Table.Put (Deliv.Name);
               Table.Put (Parent.Full_Index (True));
               Table.Put (Leader.Short_Name);
               Table.Put (Chop (Deliverable_Type_Image (Deliv.Nature)));
               Table.Put (Chop (Dissemination_Level_Image (Deliv.Dissemination)));
               Table.Put ("M" & EU_Projects.Times.Image (Deliv.Due_On));
               Table.Hline (Style = Full);
            end;
         end loop;
      end Make_Summary;

      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;

      Within_Table (Output        => To_File_Access (Target),
                    Table_Spec    => (case Style is
                                         when Full  => "|c|X|c|l|c|c|r|",
                                         when Light => "cXclccr"),
                    Callback      => Make_Summary'Access,
                    Default_Style => "",
                    Caption       => "List of deliverables \label{tbl:delivs}");

      Close (Target);
   end Print_Deliv_Summary_Low_Level;

   procedure Print_Deliv_Summary (Input  : EU_Projects.Projects.Project_Descriptor;
                                  Output : Target_Spec)
   is
      ------------------
      -- Make_Summary --
      ------------------

      procedure Make_Summary (Output : File_Access)
      is
         use EU_Projects.Nodes.Timed_Nodes.Deliverables;
         --           use type EU_Projects.Times.Instant;

         function "<" (X, Y : Deliverable_Access) return Boolean
         is (EU_Projects.Times.Instant'(X.Due_On) < Y.Due_On
             or else
                   (EU_Projects.Times.Instant'(X.Due_On) = Y.Due_On
                    and X.Full_Index (False) < Y.Full_Index (False)));

         package Deliverable_Vectors is
               new Ada.Containers.Vectors (Index_Type   => Positive,
                                           Element_Type => Deliverable_Access);

         package Deliv_Sorting is
               new Deliverable_Vectors.Generic_Sorting;

         Project_Deliverables : Deliverable_Vectors.Vector;


      begin
         for Pos in Input.All_Deliverables loop
            if Element (Pos).Status /= Parent then
               Project_Deliverables.Append (Element (Pos));
            end if;
         end loop;

         Deliv_Sorting.Sort (Project_Deliverables);

         for Deliv of Project_Deliverables loop
            declare
               use Wps;
               use Partners;

               Parent : constant Project_WP_Access :=  Project_WP_Access (Deliv.Parent_Wp);
               Leader : constant Partner_Access :=
                          Partner_Access (Input.Find (Node_Label (Parent.Leader)));
            begin
               Put (Output.all, "\delivitem");
               Put (Output.all, "{" & Deliv.Full_Index (Prefixed => True) & "}");
               Put (Output.all, "{" & Deliv.Short_Name & "}");
               Put (Output.all, "{" & Parent.Full_Index (True) & "}");
               Put (Output.all, "{" & Leader.Short_Name & "}");
               Put (Output.all, "{" & Chop (Deliverable_Type_Image (Deliv.Nature)) & "}");
               Put (Output.all, "{" & Chop (Dissemination_Level_Image (Deliv.Dissemination)) & "}");
               Put (Output.all, "{" & "M" & EU_Projects.Times.Image (Formatter, Deliv.Due_On) & "}");
               New_Line (Output.all);
            end;
         end loop;
      end Make_Summary;

      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;

      Within (Output    => To_File_Access (Target),
              Env_Name  => "deliverablelist",
              Callback  => Make_Summary'Access);

      Close (Target);
   end Print_Deliv_Summary;

   procedure Print_Compact_Deliv_Summary (Project  : EU_Projects.Projects.Project_Descriptor;
                                          Output : Target_Spec)
   is
      --------------------------
      -- Make_compact_Summary --
      --------------------------

      procedure Make_compact_Summary (Output : File_Access)
      is
         use EU_Projects.Nodes.Timed_Nodes.Deliverables;


         function Expected_Date (Deliverable : Deliverable_Access) return Instant
           with Pre => Deliverable.Status /= Clone;

         function Expected_Date (Deliverable : Deliverable_Access) return Instant
         is (case Deliverable.Status is
                when Parent =>
                   Deliverable.Clone (1).Due_On,

                when
                  Stand_Alone => Deliverable.Due_On,

                when Clone  =>
                   -- we should never arrive here
                   raise Program_Error);

         function "<" (X, Y : Deliverable_Access) return Boolean
         is (EU_Projects.Times.Instant'(Expected_Date (X)) < Expected_Date (Y)
             or else
               (EU_Projects.Times.Instant'(Expected_Date (X)) = Expected_Date (Y)
                and X.Full_Index (False) < Y.Full_Index (False)));

         package Deliverable_Vectors is
               new Ada.Containers.Vectors (Index_Type   => Positive,
                                           Element_Type => Deliverable_Access);

         package Deliv_Sorting is
               new Deliverable_Vectors.Generic_Sorting;

         Project_Deliverables : Deliverable_Vectors.Vector;

         function Expected_Date_Image (Deliv : Deliverable_Access) return String
           with Pre => Deliv.Status /= Clone;

         function Expected_Date_Image (Deliv : Deliverable_Access) return String
         is
         begin
            case Deliv.Status is
               when Clone =>
                  raise Program_Error;

               when Stand_Alone =>
                  return EU_Projects.Times.Image (Formatter, Deliv.Due_On);

               when Parent =>
                  declare
                     Result : Unbounded_String;
                  begin
                     for Idx in 1 .. Deliv.Max_Clone loop
                        if Idx > 1 then
                           Result := Result & ".";
                        end if;

                        Result := Result & EU_Projects.Times.Image (Formatter, Deliv.Clone (Idx).Due_On);
                     end loop;

                     return To_String (Result);
                  end;
            end case;
         end Expected_Date_Image;
      begin
         for Pos in Project.All_Deliverables loop
            if Element (Pos).Status /= Clone then
               Project_Deliverables.Append (Element (Pos));
            end if;
         end loop;

         Deliv_Sorting.Sort (Project_Deliverables);

         for Deliv of Project_Deliverables loop
            declare
               use Wps;
               use Partners;

               Parent_WP : constant Project_WP_Access :=  Project_WP_Access (Deliv.Parent_Wp);
               Leader : constant Partner_Access :=
                          Partner_Access (Project.Find (Node_Label (Parent_WP.Leader)));
            begin
               Put (Output.all, "\delivitem");
               Put (Output.all, "{" & Deliv.Full_Index (Prefixed => True) & "}");
               Put (Output.all, "{" & Deliv.Short_Name & "}");
               Put (Output.all, "{" & Parent_WP.Full_Index (True) & "}");
               Put (Output.all, "{" & Leader.Short_Name & "}");
               Put (Output.all, "{" & Chop (Deliverable_Type_Image (Deliv.Nature)) & "}");
               Put (Output.all, "{" & Chop (Dissemination_Level_Image (Deliv.Dissemination)) & "}");
               Put (Output.all, "{" & "M" & Expected_Date_Image (Deliv)  & "}");
               New_Line (Output.all);
            end;
         end loop;
      end Make_compact_Summary;

      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;

      Within (Output    => To_File_Access (Target),
              Env_Name  => "deliverablelist",
              Callback  => Make_Compact_Summary'Access);

      Close (Target);
   end Print_Compact_Deliv_Summary;


   procedure Print_Effort_Summary (Input  : EU_Projects.Projects.Project_Descriptor;
                                   Output : Target_Spec)
   is
      ------------------
      -- Make_Summary --
      ------------------

      procedure Make_Summary (Output : File_Access;
                              Table  : in out Table_Handler)
      is
         pragma Unreferenced (Output);
         use EU_Projects;
         use EU_Projects.Efforts;
         use EU_Projects.Nodes.Action_Nodes.WPs;
         use EU_Projects.Nodes.Partners;

         Total_Per_WP   : array (1 .. Node_Index (Input.N_WPs)) of Person_Months := (others => 0);
         Project_Effort : Person_Months := 0;
      begin
         Table.Cline (2, Input.N_WPs + 2);
         Table.Multicolumn (Span    => 1,
                            Spec    => "c|",
                            Content => "");

         for Pos in Input.All_WPs loop
            Table.Put (Element (Pos).Full_Index (True), "\effortheader");
         end loop;

         Table.Put ("Total", "\effortheader");
         Table.Hline;

         for Partner_Pos in Input.All_Partners loop
            declare

               Partner : constant Partner_Access := Element (Partner_Pos);
               Label   : constant Partner_Label := Partner_Label (Partner.Label);
               Effort  : Person_Months;
               Total   : Person_Months := 0;
            begin
               Table.Put (Partner.Short_Name, "\effortpartner");

               for WP in Input.All_WPs loop
                  Effort := Element (Wp).Effort_Of (Label);
                  Total := Total + Effort;

                  Total_Per_WP (Element (WP).Index) := Total_Per_WP (Element (WP).Index) + Effort;

                  if Element (Wp).Leader = Label then
                     Table.Put (Efforts.Person_Months'Image (Effort), "\textbf");
                  else
                     Table.Put (Efforts.Person_Months'Image (Effort));
                  end if;
               end loop;

               Table.Put (Gray & Person_Months'Image (Total));
               Table.Hline;

               Project_Effort := Project_Effort + Total;
            end;
         end loop;

         Table.Put ("Total");
         for WP in Input.All_WPs loop
            Table.Put (Gray & Person_Months'Image (Total_Per_WP (Element (WP).Index)));
         end loop;

         Table.Put (Gray & Person_Months'Image (Project_Effort));

         Table.Hline;
      end Make_Summary;

      Target : Extended_File := Open (Output);
   begin
      if Target.Class = None then
         return;
      end if;

      Within_Table_Like (Output     => To_File_Access (Target),
                         Env_Name   => "efforts",
                         Callback   => Make_Summary'Access,
                         Parameters =>
                               "Summary of staff effort"
                         and Image (Input.N_WPs + 2));

      --   (Output        => ,
      --                      Table_Spec    => "|l|*{" & Image (Input.N_WPs + 2) & "}{r|}",
      --                      Callback      => Make_Summary'Access,
      --                      Default_Style => "",
      --                      Caption       => "Summary of staff effort \label{tbl:staff}");

      Close (Target);
   end Print_Effort_Summary;

   -----------------
   -- Print_Gantt --
   -----------------

   procedure Print_Gantt_Old (Input  : EU_Projects.Projects.Project_Descriptor;
                              Output : Target_Spec)
   is
      --        use EU_Projects.Times;
      Last_Month : constant Positive :=
                     (if Input.Duration = To_Be_Decided
                      then
                         36
                      else
                         Months (Input.Duration));

      procedure Make_GANTT  (Output : File_Access;
                             Table  : in out Table_Handler)
      is
         pragma Unreferenced (Output);

         Step : constant Positive := 6;

         procedure Compact_Header_Line is
         begin
            Table.Multicolumn (2, "c", "");

            for M in 1 .. Last_Month loop
               if M mod Step = 0 then
                  Table.Multicolumn
                        (Span    => 1,
                         Spec    => "c",
                         Content => "\stilemese{" & Image (M) & "}");
               else
                  Table.Put ("");
               end if;

               Table.Put ("");
            end loop;

            Table.New_Row;
         end Compact_Header_Line;

         procedure First_Header_Line is
         begin
            Table.Put ("");
            Table.Put ("");
            for M in 1 .. Last_Month loop
               if M mod Step = 0 and M > 9 then
                  Table.Put (Image (M / 10));
               else
                  Table.Put ("");
               end if;

               Table.Put ("");
            end loop;

            Table.New_Row;
         end First_Header_Line;

         procedure Second_Header_Line is
         begin
            Table.Put ("");
            Table.Put ("");
            for M in 1 .. Last_Month loop
               if M mod Step = 0 then
                  Table.Put (Image (M mod 10));
               else
                  Table.Put ("");
               end if;

               Table.Put ("");
            end loop;

            Table.New_Row;
         end Second_Header_Line;

         procedure Show_Busy_Time (Item : EU_Projects.Nodes.Action_Nodes.Action_Node'Class)
         is

            From : constant Instant := Item.Starting_Time;
            To   : constant Instant := Item.Ending_Time;
         begin
            if From = To_Be_Decided or To = To_Be_Decided then
               for M in 1 .. Last_Month loop
                  Table.Put ("\TBDcell");

                  Table.Put ("");
               end loop;
            else
               declare
                  T : Instant;
               begin
                  for M in 1 .. Last_Month loop
                     T := To_Instant (M);

                     Table.Put ((if T >= From and then T <= To
                                then
                                   "\busytimecell"
                                else
                                   "\freetimecell"));


                     Table.Put ("");
                  end loop;
               end;
            end if;

            Table.New_Row;
         end Show_Busy_Time;

         Compact : constant Boolean := True;
      begin
         if Compact then
            Compact_Header_Line;
         else
            First_Header_Line;
            Second_Header_Line;
         end if;

         Table.Hline;

         for WP in Input.All_WPs loop
            Table.Multicolumn
                  (Span    => 2,
                   Spec    => "l|",
                   Content => "\GANTTwpname{"
                   & Element (Wp).Full_Index (True)
                   & "~"
                   & Element (WP).Short_Name
                   & "}");

            Show_Busy_Time (Element (WP).all);

            for T in Element (WP).All_Tasks loop
               Table.Put ("");
               Table.Put ("\GANTTtaskname{"
                          & WPs.Element (T).Full_Index (True)
                          & "~"
                          & WPs.Element (T).Short_Name
                          & "}");


               Show_Busy_Time (WPs.Element (T).all);
            end loop;
            Table.Hline;
         end loop;

         Table.Hline;

         declare
            use EU_Projects.Nodes.Timed_Nodes.Milestones;
            Mstone_Table    : constant Milestone_Table_Type := Input.Milestone_Table;
            Milestone_Month : array (Mstone_Table'Range (1)) of Boolean := (others => False);
         begin
            for Row in Mstone_Table'Range (2) loop
               for M  in Mstone_Table'Range (1) loop
                  if Mstone_Table (M, Row) /= null then
                     Milestone_Month (M) := True;
                  end if;
               end loop;
            end loop;

            Table.Multicolumn (2, "l|", "Milestones");
            for Has_Milestone of Milestone_Month loop
               Table.Multicolumn (1, "c", (if Has_Milestone then "\GANTTmshere" else ""));
               Table.Put ("");
            end loop;

            Table.New_Row;

            for Row in Mstone_Table'Range (2) loop
               Table.Put ("");
               Table.Put ("");

               for M in Mstone_Table'Range (1) loop
                  if Mstone_Table (M, Row) = null then
                     Table.Put ("");
                  else
                     Table.Multicolumn (1, "c", "\GANTTmsstyle"
                                        & "{" & Mstone_Table (M, Row).Full_Index (True) & "}");
                  end if;

                  Table.Put ("");
               end loop;

               Table.New_Row;
            end loop;
         end;
      end Make_GANTT;

      Target : Extended_File := Open (Output);

   begin
      if Target.Class = None then
         return;
      end if;


      Within_Table (Output        => To_File_Access (Target),
                    Table_Spec    => "p{1em}l|"
                    & "*{" & Image (Last_Month) & "}"
                    & "{p{\larghezzacella}p{\larghezzasep}@{}}",
                    Callback      => Make_GANTT'Access,
                    Default_Style => "",
                    Caption       => "GANTT \label{tbl:gantt}");

      Close (Target);
   end Print_Gantt_Old;

   ------------------------
   -- Longest_Label_Size --
   ------------------------

   procedure Longest_Label_Size (Input             : EU_Projects.Projects.Project_Descriptor;
                                 WP_Label_Size     : out Natural;
                                 Task_Label_Size   : out Natural;
                                 Wp_And_Task_Count : out Natural)
   is
   begin
      WP_Label_Size := 0;
      Task_Label_Size := 0;
      Wp_And_Task_Count := 0;

      for Wp in Input.All_WPs loop
         WP_Label_Size := Integer'Max (WP_Label_Size,
                                       Element (Wp).Short_Name'Length);

         Wp_And_Task_Count := Wp_And_Task_Count + 1;

         for Tsk in Element (Wp).All_Tasks loop
            Task_Label_Size := Integer'Max (Task_Label_Size,
                                            WPs.Element (Tsk).Short_Name'Length);

            Wp_And_Task_Count := Wp_And_Task_Count + 1;

         end loop;
      end loop;

      --        Put_Line ("WW=" & Wp_And_Task_Count'Image);
   end Longest_Label_Size;

   -----------------
   -- Print_Gantt --
   -----------------


   type Graphic_Setup_Descriptor is
      record
         Task_X0         : Picture_Length;
         Month_Step      : Month_Number;
         Label_Size      : Picture_Length;
         Month_Width     : Picture_Length;
         Line_Heigth     : Picture_Length;
         Pre_Skip        : Picture_Length;
         Post_Skip       : Picture_Length;
         Interline       : Picture_Length;
         Unit_Length     : Latex_Length;
         Last_Month      : Month_Number;
         Top_Position    : Picture_Length;
         Total_Width     : Picture_Length;
         Small_Interline : Picture_Length;
         Grid_Height     : Picture_Length;
         Tick_Length     : Picture_Length;
         Top_WP          : Picture_Length;
         Header_Skip     : Picture_Length;
         Font_Height     : Picture_Length;
      end record;

   procedure Make_GANTT  (Input          : EU_Projects.Projects.Project_Descriptor;
                          Output         : File_Access;
                          Graphic_Setup  : Graphic_Setup_Descriptor;
                          Parameters     : GANTT_Parameters)
   is
      use EU_Projects;
      --        use EU_Projects.Times;

      procedure Make_WP_Separator;

      Current_V_Pos : Picture_Length;

      function Month_Position (Month : Projects.Month_Number;
                               Setup : Graphic_Setup_Descriptor)
                               return Picture_Length
      is (Setup.Label_Size + (Integer (Month) - 1) * Setup.Month_Width);

      function To_Length (L     : Picture_Length;
                          Setup : Graphic_Setup_Descriptor)
                          return Latex_Length
      is (Float (L) * Setup.Unit_Length);

      procedure Next_Row is
      begin
         --              Put_Line (Current_V_Pos'Image);
         --              Put_Line (Graphic_Setup.Month_Heigth'Image);
         --              Put_Line (Graphic_Setup.Interline'image);
         Current_V_Pos := Current_V_Pos - Graphic_Setup.Line_Heigth;
      end Next_Row;

      procedure Show_Busy_Time (Item      : EU_Projects.Nodes.Action_Nodes.Action_Node'Class;
                                Style     : String;
                                Intensity : EU_Projects.Nodes.Action_Nodes.Tasks.Task_Intensity)
      is

         procedure Make_Bar (From, To       : Month_Number;
                             Command        : String;
                             Show_Intensity : Boolean) is
            Start : constant Picture_Length :=
                      Month_Position (From, Graphic_Setup);

            Len   : constant Latex_Length :=
                      To_Length (Month_Position (To, Graphic_Setup)-Start, Graphic_Setup);

            Shrink : constant Float := 0.8;
            H      : constant Latex_Length :=
                       Shrink * To_Length (Graphic_Setup.Line_Heigth, Graphic_Setup);

            H2 : constant Latex_Length := Float'Max (Intensity, 0.15) * H;

            Box_Raise : constant Latex_Length :=
                          (1.0 - Shrink) * 0.5 * To_Length (Graphic_Setup.Line_Heigth, Graphic_Setup);
         begin
            if Show_Intensity  then
               Put_Line (Output.all, Put (X    => Start,
                                          Y    => Current_V_Pos,
                                          What => Style
                                          & "{"
                                          & (
                                                "\shadedrule"
                                                & "[" & Image (Box_Raise) & "]"
                                                & "{" & Image (Len) & "}"
                                                & "{" & Image (H) & "}"
                                               )
                                          & "}"));

               Put_Line (Output.all, Put (X    => Start,
                                          Y    => Current_V_Pos,
                                          What => Style
                                          & "{"
                                          & (
                                                Command
                                                & "[" & Image (Box_Raise) & "]"
                                                & "{" & Image (Len) & "}"
                                                & "{" & Image (H2) & "}"
                                               )
                                          & "}"));
            else
               Put_Line (Output.all, Put (X    => Start,
                                          Y    => Current_V_Pos,
                                          What => Style
                                          & "{"
                                          & (
                                                Command
                                                & "[" & Image (Box_Raise) & "]"
                                                & "{" & Image (Len) & "}"
                                                & "{" & Image (H) & "}"
                                               )
                                          & "}"));
            end if;

            declare
               Dx : constant Picture_Length := Picture_Length (Len / Graphic_Setup.Unit_Length);
               Dy : constant Picture_Length := Picture_Length (H / Graphic_Setup.Unit_Length);
               B  : constant Picture_Length :=  Picture_Length (Box_Raise / Graphic_Setup.Unit_Length);
               Y0 : constant Picture_Length := Current_V_Pos + B;
            begin
               Put_Line (Output.all, Put (X => Start,
                                          Y    => Y0,
                                          What => HLine (Dx)));


               Put_Line (Output.all, Put (X => Start,
                                          Y    => Y0,
                                          What => Vline (Dy)));

               Put_Line (Output.all, Put (X => Start,
                                          Y    => Y0 + Dy,
                                          What => HLine (Dx)));


               Put_Line (Output.all, Put (X => Start + Dx,
                                          Y    => Y0,
                                          What => VLine (Dy)));
            end;
         end Make_Bar;

         From  : constant Instant := Item.Starting_Time;
         To    : constant Instant := Item.Ending_Time;
      begin
         if From = To_Be_Decided or To = To_Be_Decided then
            Make_Bar (From           => 1,
                      To             => Graphic_Setup.Last_Month,
                      Command        => "\lightshadedrule",
                      Show_Intensity => False);
         else
            Make_Bar (From           => Projects.Month_Number (Months (From)),
                      To             => Projects.Month_Number (Months (To)),
                      Command        => "\rule",
                      Show_Intensity => Intensity < 0.99);
         end if;
      end Show_Busy_Time;

      procedure Make_Label (Command : String;
                            Item    : EU_Projects.Nodes.Action_Nodes.Action_Node'Class;
                            Indent  : Picture_Length)
      is
      begin
         Put_Line (Output.all,
                   Picture.Put (X    => Indent,
                                Y    => Current_V_Pos,
                                What => Command
                                & "{" & Item.Full_Index (True) & "~" & Item.Short_Name & "}"));
      end Make_Label;

      procedure Mark_Month_With_Milestone (Month : Projects.Month_Number) is
      begin
         Put_Line (Output.all,
                   Picture.Text (X       => Month_Position (Month, Graphic_Setup),
                                 Y       => Current_V_Pos,
                                 Content => "\GANTThasmilestone"));
      end Mark_Month_With_Milestone;

      procedure Put_Milestone (Month  : Month_Number;
                               Mstone : Milestones.Milestone_Access)
      is
      begin
         Put_Line (Output.all,
                   Text (X       => Month_Position (Month, Graphic_Setup),
                         Y       => Current_V_Pos,
                         Content => Hbox ("\GANTTmstoneNumber "
                               & Mstone.Full_Index (False)
                               & ";")));
      end Put_Milestone;

      procedure Mark_Milestones
      is
         use EU_Projects.Nodes.Timed_Nodes.Milestones;
         Mstone_Table  : constant Milestone_Table_Type := Input.Milestone_Table;
         Has_Milestone : array (Mstone_Table'Range (1)) of Boolean := (others => False);
      begin
         Make_WP_Separator;

         Put_Line (Output.all,
                   Picture.Put (X    => 0.0,
                                Y    => Current_V_Pos,
                                What => "\milestonelabel"));


         for Row in Mstone_Table'Range (2) loop
            for Month  in Mstone_Table'Range (1) loop
               if Mstone_Table (Month, Row) /= null then
                  Has_Milestone (Month) := True;
               end if;
            end loop;
         end loop;

         for Month in Has_Milestone'Range loop
            if Has_Milestone (Month) then
               Mark_Month_With_Milestone (Month);
            end if;
         end loop;

         Next_Row;

         for Row in Mstone_Table'Range (2) loop

            for Month in Mstone_Table'Range (1) loop
               if Mstone_Table (Month, Row) /= null then
                  Put_Milestone (Month, Mstone_Table (Month, Row));
               end if;
            end loop;

            Next_Row;
         end loop;
      end Mark_Milestones;

      procedure Make_Header_And_Grid (Setup : Graphic_Setup_Descriptor)
      is
         Current_Month : Month_Number;

         function Skip (Month : Month_Number) return Picture_Length
         is
         begin
            return Setup.Header_Skip +  (if Month mod 12 = 0 then
                                            0.0
                                         elsif Month mod 6 = 0 then
                                            0.4 * Setup.Tick_Length
                                         elsif Month mod 3 = 0 then
                                            0.8 * Setup.Tick_Length
                                         else
                                            0.8 * Setup.Tick_Length);
         end Skip;
      begin
         Current_Month := Setup.Month_Step;

         while Current_Month <= Setup.Last_Month loop
            Put_Line (Output.all,
                      Picture.Put (X    => Month_Position (Current_Month, Setup),
                                   Y    => Current_V_Pos,
                                   What => Hbox (Chop (Current_Month'Image))));

            Current_Month := Current_Month + Setup.Month_Step;
         end loop;

         for Month in 1 .. Setup.Last_Month loop
            Put (Output.all, "{");
            Put (Output.all,
                 (if Month mod 12 = 0 then
                     "\xiithick"
                  elsif Month mod 6 = 0 then
                     "\vithick"
                  elsif Month mod 3 = 0 then
                     "\iiithick"
                  else
                     "\ithick"));

            Put (Output.all,
                 Picture.VLine (X         => Month_Position (Month, Setup),
                                Y         => 2 * Setup.Line_Heigth,
                                Length    => Current_V_Pos - Skip (Month)-2 * Setup.Line_Heigth,
                                Direction => Up));

            Put_Line (Output.all, "}");
         end loop;
      end Make_Header_And_Grid;

      procedure Make_WP_Separator
      is
      begin
         Put_Line (Output.all,
                   Picture.Hline
                         (X      =>
                                0.0,
                          Y      =>
                                Current_V_Pos
                          + Graphic_Setup.Font_Height
                          + Graphic_Setup.Small_Interline,

                          Length =>
                                Month_Position (Graphic_Setup.Last_Month, Graphic_Setup)));
      end Make_WP_Separator;

      procedure Show_Deliverables (Wp : WPs.Project_WP)
      is
         use type Deliverables.Deliverable_Status;

         package Instant_Sets is
               new Ada.Containers.Ordered_Sets (Times.Instant);

         Active_Months : Instant_Sets.Set;
         H             : constant Latex_Length :=
                           0.5 * To_Length (Graphic_Setup.Line_Heigth, Graphic_Setup);

      begin
         for Pos in  Wp.All_Deliverables loop
            if WPs.Element (Pos).Status /= Deliverables.Parent then
               Active_Months.Include (WPs.Element (Pos).Due_On);
            end if;
         end loop;

         for M of Active_Months loop
            if M /= To_Be_Decided then
               Put_Line (Output.all,
                         Picture.Put (X    => Month_Position (Month_Number (Months (M)), Graphic_Setup),
                                      Y    => Current_V_Pos + Picture_Length (H / Graphic_Setup.Unit_Length),
                                      What => "\GANTTdeliv{" & Image (0.9 * H) & "}"));
            end if;
         end loop;
      end Show_Deliverables;
   begin
      Current_V_Pos := Graphic_Setup.Top_Position;

      Make_Header_And_Grid (Graphic_Setup);

      Current_V_Pos := Graphic_Setup.Top_WP;

      for WP in Input.All_WPs loop
         Make_WP_Separator;

         Make_Label (Command => "\GANTTwpname",
                     Item    => Element (WP).all,
                     Indent  => 0.0);

         Show_Busy_Time (Element (WP).all, "\GANTTwpBarStyle", 1.0);

         if Parameters.Show_Deliverables then
            Show_Deliverables (Element (Wp).all);
         end if;

         Next_Row;

         for T in Element (WP).All_Tasks loop
            Make_Label (Command => "\GANTTtaskname",
                        Item    => WPs.Element (T).all,
                        Indent  => Graphic_Setup.Task_X0);

            Show_Busy_Time (WPs.Element (T).all, "\GANTTtaskBarStyle", Wps.Element (T).Intensity);
            Next_Row;
         end loop;
      end loop;

      Mark_Milestones;
   end Make_GANTT;

   procedure Print_Gantt (Input      : EU_Projects.Projects.Project_Descriptor;
                          Target     : Target_Spec;
                          Parameters : GANTT_Parameters)
   is
      --        use EU_Projects.Times;


      --
      -- We use as unit length in the picture environment 0.5em that is
      -- (we measured) approximately the average letter length.  1em is
      -- approximately the font size, so we use half the font size
      -- as \unitlength.
      --



      Extended_Output    : Extended_File := Open (Target);
      Output             : File_Access;
      Longest_WP_Label   : Natural;
      Longest_Task_Label : Natural;
      Wp_And_Task_Count  : Natural;
      Task_Indent        : constant Positive := 1;
      Last_Month         : constant Month_Number :=
                             (if Input.Duration = To_Be_Decided
                              then
                                 36
                              else
                                 Month_Number (Months (Input.Duration)));


   begin
      if Extended_Output.Class = None then
         return;
      else
         Output := To_File_Access (Extended_Output);
      end if;

      Longest_Label_Size (Input             => Input,
                          WP_Label_Size     => Longest_WP_Label,
                          Task_Label_Size   => Longest_Task_Label,
                          Wp_And_Task_Count => Wp_And_Task_Count);


      declare
         --
         -- We checked experimentally that the average width of a char
         -- is half the font size.  This allows us a crude estimation of
         -- text size.
         --
         Average_Font_Size : constant Latex_Length := 0.5 * Parameters.Font_Size;



         --
         -- We choose the picture \unitlength so that the average letter
         -- width is equal to Averave_Font_Picture_Size in picture units
         --
         Unit_Length       : constant Latex_Length := 1.0 * Pt;

         Font_Width        : constant Picture_Length :=
                               Picture_Length (Average_Font_Size / Unit_Length);

         Font_Height       : constant Picture_Length :=
                               Picture_Length (Parameters.Font_Size / Unit_Length);

         Picture_Width     : constant Picture_Length :=
                               Picture_Length (Parameters.Textwidth / Unit_Length);

         Pre_Skip          : constant Picture_Length :=
                               Picture_Length (Parameters.Pre_Label_Skip * Parameters.Font_Size / Unit_Length);

         Post_Skip         : constant Picture_Length :=
                               Picture_Length (Parameters.Post_Label_Skip * Parameters.Font_Size / Unit_Length);

         Longest_Label     : constant Positive :=
                               Integer'Max (Longest_WP_Label,  Task_Indent + Longest_Task_Label);

         pragma Warnings (Off, "static fixed-point value is not a multiple of Small");

         Label_Width       : constant Picture_Length :=
                               Longest_Label * Font_Width;

         Month_Width       : constant Picture_Length :=
                               (Picture_Width - Label_Width) / Picture_Length (Last_Month);

         Line_Height      : constant Picture_Length :=
                              Pre_Skip + Font_Height + Post_Skip;

         Header_Skip      : constant Picture_Length :=
                              Picture_Length (Parameters.Header_Skip * Parameters.Font_Size / Unit_Length);

         Tick_Length      : constant Picture_Length :=
                              Picture_Length (Parameters.Tick_Length * Parameters.Font_Size / Unit_Length);

         Header_Height    : constant Picture_Length :=
                              Font_Height + Tick_Length + Header_Skip;

         Grid_Height       : constant Picture_Length :=
                               Wp_And_Task_Count * Line_Height;

         Picture_Heigth    : constant Picture_Length :=
                               Header_Height + Grid_Height + 3.0 * Line_Height;

         Graphic_Setup     : constant Graphic_Setup_Descriptor :=
                               Graphic_Setup_Descriptor'
                                     (Task_X0         => Parameters.Task_Indent * Font_Width,
                                      Month_Step      => 3,
                                      Label_Size      => Label_Width,
                                      Month_Width     => Month_Width,
                                      Line_Heigth     => Line_Height,
                                      Unit_Length     => Unit_Length,
                                      Last_Month      => Last_Month,
                                      Pre_Skip        => Pre_Skip,
                                      Post_Skip       => Post_Skip,
                                      Interline       => Pre_Skip + Post_Skip,
                                      Small_Interline => Pre_Skip * 0.25,
                                      Top_Position    => Picture_Heigth - Font_Height,
                                      Total_Width     => Picture_Width,
                                      Grid_Height     => Grid_Height,
                                      Tick_Length     => Tick_Length,
                                      Top_WP          => (Wp_And_Task_Count - 1 + 3) * Line_Height,
                                      Header_Skip     => Header_Skip,
                                      Font_Height     => Font_Height);


         procedure Make_Picture (Output : File_Access) is
            procedure Make_GANTT_Gateway (Output : File_Access) is
            begin
               Make_GANTT (Input, Output, Graphic_Setup, Parameters);
            end Make_GANTT_Gateway;
         begin
            Put_Line (Output.all, "\setlength{\unitlength}{" & Image (Unit_Length, Pt) & "}");
            Put (Output.all, "\rotategannt{");
            Within_Picture (Output   => Output,
                            Width    => Picture_Width,
                            Heigth   => Picture_Heigth,
                            Callback => Make_GANTT_Gateway'Access);
            Put_Line (Output.all, "}");
         end Make_Picture;
      begin
         Within (Output    => Output,
                 Env_Name  => "gantt",
                 Callback  => Make_Picture'Access,
                 Parameter => "\GANTTcaption");

         --           Put (Output.all, "\begin{gantt}{\GANTTcaption}");
         --           Put (Output.all, "\centering");
         --           Put_Line (Output.all, "\setlength{\unitlength}{" & Image (Unit_Length, Pt) & "}");
         --           Within_Picture (Output   => Output,
         --                           Width    => Picture_Width,
         --                           Heigth   => Picture_Heigth,
         --                           Callback => Make_GANTT_Gateway'Access);

         --           Put (Output.all, "\end{gantt}");

      end;



      Close (Extended_Output);
   end Print_Gantt;
   -------------
   -- Process --
   -------------

   overriding procedure Process
         (Processor : Processor_Type;
          Input     : EU_Projects.Projects.Project_Descriptor)
   is
   begin
      Print_Partners (Input, Processor.Partner_Target);

      Print_WPs (Input, Processor.Wp_Target);

      Print_WP_Summary (Input, Processor.WP_Summary_Target, Light);

      Print_Milestones (Input, Processor.Milestone_Target, Full);

      if True then
         Print_Deliv_Summary (Input, Processor.Deliv_Summary_Target);
      else
         Print_Deliv_Summary_Low_Level (Input, Processor.Deliv_Summary_Target, Light);
      end if;

      Print_Compact_Deliv_Summary (Input, Processor.Deliv_Compact_Summary_Target);

      Print_Effort_Summary (Input, Processor.Effort_Summary_Target);

      if False then
         Print_Gantt_Old (Input, Processor.Gantt_Target);
      else
         Print_Gantt (Input      => Input,
                      Target     => Processor.Gantt_Target,
                      Parameters => Processor.Gantt_Style);
      end if;
   end Process;
begin
   Processor_Tables.Register (ID  => To_Id ("latex"),
                              Tag => Processor_Type'Tag);
end Project_Processor.Processors.LaTeX;

