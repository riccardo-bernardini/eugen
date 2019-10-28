with EU_Projects.Projects;
with Ada.Strings.Bounded;
with Latex_Writer; use Latex_Writer;

package Project_Processor.Processors.LaTeX is

   type Processor_Type is
     new Abstract_Processor
   with
     private;


   overriding function Create (Params : not null access Processor_Parameter)
                               return Processor_Type;


   overriding procedure Process
     (Processor : Processor_Type;
      Input     : EU_Projects.Projects.Project_Descriptor);
private
   package Bounded_Filenames is
     new Ada.Strings.Bounded.Generic_Bounded_Length (2048);

   subtype Bounded_Filename is Bounded_Filenames.Bounded_String;

   function Bless (X : String) return Bounded_Filename
   is (Bounded_Filenames.To_Bounded_String (X));

   function To_S (X : Bounded_Filename) return String
   is (Bounded_Filenames.To_String (X));


   type Target_Class is (File, Standard_Output, None);
   type Target_Spec (Class : Target_Class := File) is
      record
         case Class is
         when None | Standard_Output =>
            null;

         when File =>
            Filename : Bounded_Filename;
         end case;
      end record;

   type GANTT_Parameters is
      record
         Font_Size         : Latex_Length;
         Textwidth         : Latex_Length;
         Pre_Label_Skip    : Em_Length;
         Post_Label_Skip   : Em_Length;
         Header_Skip       : Em_Length;
         Tick_Length       : Em_Length;
         Task_Indent       : Natural;
         Show_Deliverables : Boolean;
      end record;

   type Processor_Type is
     new Abstract_Processor
   with
      record
         Wp_Target                     : Target_Spec;
         Wp_Summary_Target             : Target_Spec;
         Partner_Target                : Target_Spec;
         Deliverable_Target            : Target_Spec;
         Deliv_Summary_Target          : Target_Spec;
         Deliv_Compact_Summary_Target  : Target_Spec;
         Milestone_Target              : Target_Spec;
         Effort_Summary_Target         : Target_Spec;
         Gantt_Target                  : Target_Spec;
         Gantt_Style                   : GANTT_Parameters;
      end record;
end Project_Processor.Processors.LaTeX;
