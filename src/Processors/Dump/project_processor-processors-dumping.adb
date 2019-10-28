pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;

with EU_Projects.Nodes.Partners;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Action_Nodes.Tasks;

with Project_Processor.Processors.Processor_Tables;
with EU_Projects.Times;

package body Project_Processor.Processors.Dumping is
   use EU_Projects.Projects;
   ------------
   -- Create --
   ------------

   overriding function Create
     (Params : not null access Processor_Parameter) return Processor_Type
   is
      pragma Unreferenced (Params);
      Result:Processor_Type;
   begin
      return Result;
   end Create;

   procedure Print_Partners (Input : EU_Projects.Projects.Project_Descriptor)
   is
      use EU_Projects.Nodes.Partners;

      procedure Print_Partner (Partner : Partner_Access) is
         use EU_Projects.Nodes;
      begin
         Put_Line ("[PARTNER]");
         Put_Line ("  Name : " & Partner.Name);
         Put_Line ("  Label : " & To_String (Partner.Label));
         Put_Line ("  short : " & Partner.Short_Name);
         Put_Line ("[/ PARTNER]");
      end Print_Partner;
   begin
      for Idx in Input.All_Partners loop
         Print_Partner (Element (Idx));
      end loop;
   end Print_Partners;

   procedure Print_WPs (Input : EU_Projects.Projects.Project_Descriptor)
   is
      use EU_Projects.Nodes.Action_Nodes.WPs;
      use EU_Projects.Nodes.Action_Nodes.Tasks;

      procedure Print_WP (WP : Project_WP_Access) is
         use EU_Projects.Nodes;

         procedure Print_Times (N : Action_Nodes.Action_Node'Class; Tab : String) is
            use EU_Projects.Times;
         begin
            Put_Line (Tab & "begin : " & Image (N.Starting_Time));
            Put_Line (Tab & "end   : " & Image (N.Ending_Time));
         end Print_Times;

         procedure Print_Efforts (N : Action_Nodes.Action_Node'Class; Tab : String) is
         begin
            Put_Line (Tab & "[efforts]");
            for Pos in Input.All_Partners loop
               Put_Line (Tab & "  "
                         & To_String (Element (Pos).Label)
                         & " : "
                         & N.Effort_Of (Partners.Partner_Label (Element (Pos).Label))'image);
            end loop;

            Put_Line (Tab & "[/ efforts]");
         end Print_Efforts;

         procedure Print_Task (Tsk : Project_Task_Access) is
         begin
            Put_Line ("   [Task]");
            Put_Line ("     Name : " & tsk.Name);
            Put_Line ("     Label : " & To_String (tsk.Label));
            Put_Line ("     short : " & tsk.Short_Name);

            Print_Times (Tsk.all, "     ");
            Print_Efforts (Tsk.all, "     ");

            Put_Line ("   [/ Task]");
         end Print_Task;
      begin
         Put_Line ("[WP]");
         Put_Line ("  Name : " & WP.Name);
         Put_Line ("  Label : " & To_String (WP.Label));
         Put_Line ("  short : " & WP.Short_Name);

         Print_Times (WP.all, "  ");
         Print_Efforts (WP.all, "  ");

         for Idx in WP.All_Tasks loop
            Print_Task (Element (Idx));
         end loop;

         Put_Line ("[/ WP]");
      end Print_WP;
   begin
      for Idx in Input.All_WPs loop
         Print_WP (Element (Idx));
      end loop;
   end Print_WPs;
   -------------
   -- Process --
   -------------

   overriding procedure Process
     (Processor : Processor_Type;
      Input     : EU_Projects.Projects.Project_Descriptor)
   is
      pragma Unreferenced (Processor);
   begin
      Print_Partners (Input);
      Print_WPs (Input);
--        Print_Deliverables (Input);
--        Print_Milestones (Input);
   end Process;
begin
   Processor_Tables.Register (ID  => To_Id ("dump"),
                              Tag => Processor_Type'Tag);
end Project_Processor.Processors.Dumping;
