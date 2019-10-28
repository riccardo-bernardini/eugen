with Project_Processor.Parsers.Abstract_Parsers;
with Plugins.Tables;


package Project_Processor.Parsers.Parser_Tables is
  new Plugins.Tables (Root_Plugin_Type  => Abstract_Parsers.Abstract_Parser,
                      Plugin_Parameters => Plugins.Parameter_Maps.Map,
                      Plugin_ID         => Parser_ID,
                      Constructor       => Abstract_Parsers.Create);
