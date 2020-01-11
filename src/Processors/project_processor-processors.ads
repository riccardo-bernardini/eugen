with EU_Projects.Projects;
with Ada.Strings.Bounded;
with Plugins;

package Project_Processor.Processors is
   package Processor_Names is
     new Ada.Strings.Bounded.Generic_Bounded_Length (16);

   subtype Processor_Parameter is Plugins.Parameter_Map;
   subtype Processor_Parameter_Access is Plugins.Parameter_Map_Access;

   type Processor_ID is new Processor_Names.Bounded_String;

   function To_Id (X : String) return Processor_ID
   is (Processor_ID (Processor_Names.To_Bounded_String (X)));

   function Image (X : Processor_ID) return String
   is (Processor_Names.To_String (Processor_Names.Bounded_String(X)));

    type Abstract_Processor is interface;


   function Create (Params : not null access Processor_Parameter)
                    return Abstract_Processor is abstract;


   procedure Process
     (Processor : Abstract_Processor;
      Input     : EU_Projects.Projects.Project_Descriptor)
   is abstract;

   Processor_Error : exception;
end Project_Processor.Processors;
