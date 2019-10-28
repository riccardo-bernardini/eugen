with Project_Processor.Processors;
with Project_Processor.Parsers;

package Project_Processor.Configuration is
   procedure Initialize;

   type Processing_Call is
      record
         Name       : Processors.Processor_ID;
         Parameters : Processors.Processor_Parameter_Access;
      end record;

   procedure For_All_Calls
         (Callback : not null access procedure (Call : Processing_Call));

   function Input_Data return String;

   function Input_Format return Parsers.Parser_ID;

   function Parser_Parameters return Parsers.Parser_Parameter_Access;

   Bad_Command_Line : exception;
end Project_Processor.Configuration;
