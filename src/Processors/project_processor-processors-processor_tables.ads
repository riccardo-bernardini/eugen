with Plugins.Tables;

package Project_Processor.Processors.Processor_Tables is
  new Plugins.Tables (Root_Plugin_Type  => Abstract_Processor,
                      Plugin_Parameters => Processor_Parameter,
                      Plugin_ID         => Processor_ID,
                      Constructor       => Create);
