pragma Ada_2012;
with Ada.Containers.Indefinite_Ordered_Maps;
with Project_Processor.Parsers.Abstract_Parsers;

package body Project_Processor.Parsers is

   package Extension_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => string,
                                                 Element_Type => Parser_ID);

   Extension_To_Parser : Extension_Maps.Map;

   -------------------
   -- Parse_Project --
   -------------------

   function Parse_Project
     (Input      : String;
      Format     : Parser_ID;
      Parameters : Parser_Parameter_access)
      return EU_Projects.Projects.Project_Descriptor
   is
      use Abstract_Parsers;

      Parser : Abstract_Parser'Class := Get_Parser (Format, Parameters);
   begin
      return Project : EU_Projects.Projects.Project_Descriptor do
         Parser.Parse (Project, Input);
      end return;
   end Parse_Project;

   -----------------
   -- Find_Parser --
   -----------------

   function Find_Parser (File_Extension : String) return Parser_ID
   is
      use Extension_Maps;

      Pos : constant Cursor := Extension_To_Parser.Find (File_Extension);
   begin
      if Pos = No_Element then
         return No_Parser;

      else
         return Extension_To_Parser (Pos);

      end if;
   end Find_Parser;


   ------------------------
   -- Register_Extension --
   ------------------------

   procedure Register_Extension (Parser         : Parser_ID;
                                 File_Extension : String)
   is
   begin
      if Extension_To_Parser.Contains (File_Extension) then
         raise Constraint_Error with "Extension '" & File_Extension & "' already known";
      end if;

      Extension_To_Parser.Include (Key      => File_Extension,
                                   New_Item => Parser);
   end Register_Extension;
end Project_Processor.Parsers;
