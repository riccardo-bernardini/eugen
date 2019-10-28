pragma Ada_2012;

with Project_Processor.Parsers.Parser_Tables;

package body Project_Processor.Parsers.Abstract_Parsers is

   --------------
   -- Register --
   --------------

   procedure Register
         (Format  : Parser_ID;
          Tag     : Ada.Tags.Tag)
   is
   begin
      Parser_Tables.Register (Format, Tag);
   end Register;

   ----------------
   -- Get_Parser --
   ----------------

   function Get_Parser
         (Format     : Parser_ID;
          Parameters : Parser_Parameter_access)
      return Abstract_Parser'Class
   is
   begin
      return Parser_Tables.Get (ID                => Format,
                                Params            => Parameters,
                                Search_If_Missing => False);
   end Get_Parser;


   function Is_Known (Format : Parser_ID) return Boolean
   is (Parser_Tables.Exists (Format));

end Project_Processor.Parsers.Abstract_Parsers;
