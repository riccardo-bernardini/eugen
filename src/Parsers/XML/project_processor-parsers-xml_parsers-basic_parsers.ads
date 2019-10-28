with DOM.Core;

private
package Project_Processor.Parsers.XML_Parsers.Basic_Parsers is

   function Expect_ID (N    : DOM.Core.Node;
                       Name : String := "label")
                       return EU_Projects.Dotted_Identifier;

   function Expect_Number (N    : DOM.Core.Node;
                           Name : String)
                           return Natural;

   function Expect_Time (N    : DOM.Core.Node;
                         Name : String)
                         return Symbolic_Instant;
   function Expect_Duration (N    : DOM.Core.Node;
                             Name : String)
                             return Symbolic_Duration;

   procedure Get_End_And_Duration
     (N          : DOM.Core.Node;
      Start_Time : in Symbolic_Instant;
      End_Time   : out Symbolic_Instant;
      Duration   : out Symbolic_Duration);



end Project_Processor.Parsers.XML_Parsers.Basic_Parsers;


