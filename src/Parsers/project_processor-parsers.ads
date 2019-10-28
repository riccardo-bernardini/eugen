with Plugins;
with EU_Projects.Projects;


package Project_Processor.Parsers is
   subtype Parser_Parameter is Plugins.Parameter_Map;
   subtype Parser_Parameter_access is Plugins.Parameter_Map_Access;

   type Parser_ID is new String;

   No_Parser : constant Parser_ID := "";

   function Find_Parser (File_Extension : String) return Parser_ID;

   function Parse_Project (Input      : String;
                           Format     : Parser_ID;
                           Parameters : Parser_Parameter_access)
                           return EU_Projects.Projects.Project_Descriptor;

   Parsing_Error : exception;
private

   procedure Register_Extension (Parser         : Parser_ID;
                                 File_Extension : String);

end Project_Processor.Parsers;
