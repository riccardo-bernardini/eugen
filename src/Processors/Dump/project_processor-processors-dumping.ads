with EU_Projects.Projects;

package Project_Processor.Processors.Dumping is

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
   type Processor_Type is
     new Abstract_Processor
   with
     null record;
end Project_Processor.Processors.Dumping;
