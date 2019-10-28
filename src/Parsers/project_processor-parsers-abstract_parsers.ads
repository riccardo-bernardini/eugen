with Ada.Tags;

package Project_Processor.Parsers.Abstract_Parsers is
   type Abstract_Parser is interface;


   function Create (Params : not null access Plugins.Parameter_Maps.Map)
                    return Abstract_Parser is abstract;

   procedure Parse
         (Parser    : in out Abstract_Parser;
          Project   :    out EU_Projects.Projects.Project_Descriptor;
          Input     : String)
   is abstract;

   procedure Register (Format  : Parser_ID;
                       Tag     : Ada.Tags.Tag)
         with Pre => Ada.Tags.Is_Descendant_At_Same_Level (Descendant => Tag,
                                                           Ancestor   => Abstract_Parser'Tag);

   function Get_Parser (Format     : Parser_ID;
          Parameters : Parser_Parameter_access)
                        return Abstract_Parser'Class;

   function Is_Known (Format : Parser_ID) return Boolean;
end Project_Processor.Parsers.Abstract_Parsers;
