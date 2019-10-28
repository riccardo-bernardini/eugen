with XML_Utilities;
package body Project_Processor.Parsers.XML_Parsers.Basic_Parsers is

   ---------------
   -- Expect_ID --
   ---------------

   function Expect_ID (N    : DOM.Core.Node;
                       Name : String := "label")
                    return EU_Projects.Dotted_Identifier
   is
      use XML_Utilities;

      S : constant String := Expect_Attribute (N, Name);
   begin
      if not EU_Projects.Is_Valid_ID (S) then
         raise Parsing_Error;
      end if;

      return EU_Projects.To_ID (S);
   end Expect_ID;

   -------------------
   -- Expect_Number --
   -------------------

   function Expect_Number (N    : DOM.Core.Node;
                           Name : String)
                           return Natural
   is
      use XML_Utilities;

      S : constant String := Expect_Attribute (N, Name);
   begin
      return Natural'Value (S);
   end Expect_Number;

   -----------------
   -- Expect_Time --
   -----------------

   function Expect_Time (N    : DOM.Core.Node;
                         Name : String)
                      return Symbolic_Instant
   is
      use XML_Utilities;

   begin
      return Parse (Expect_Attribute (N, Name));
   end Expect_Time;

   ---------------------
   -- Expect_Duration --
   ---------------------

   function Expect_Duration (N    : DOM.Core.Node;
                             Name : String)
                          return Symbolic_Duration
   is
      use XML_Utilities;

   begin
      return Parse (Expect_Attribute (N, Name));
   end Expect_Duration;

   --------------------------
   -- Get_End_And_Duration --
   --------------------------

   procedure Get_End_And_Duration
         (N          : DOM.Core.Node;
          Start_Time : in Symbolic_Instant;
          End_Time   : out Symbolic_Instant;
          Duration   : out Symbolic_Duration)
   is
      use XML_Utilities;

      End_Given      : constant Boolean := Has_Attribute (N, "end");
      Duration_Given : constant Boolean := Has_Attribute (N, "duration");
   begin
      if End_Given and Duration_Given then
         raise Parsing_Error;

      elsif (not End_Given) and (not Duration_Given) then
         raise Parsing_Error;

      elsif End_Given and (not Duration_Given) then
         End_Time := Expect_Time (N, "end");
         Duration := End_Time - Start_Time;

      else
         Duration := Expect_Duration (N, "duration");
         End_Time := Duration + Start_Time;

      end if;
   end Get_End_And_Duration;



end Project_Processor.Parsers.XML_Parsers.Basic_Parsers;
