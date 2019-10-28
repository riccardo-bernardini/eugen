with Ada.Unchecked_Deallocation;
with DOM.Core.Nodes;
with Ada.Text_IO; use Ada.Text_IO;

package body XML_Scanners is

   -------------------
   -- Child_Scanner --
   -------------------

   function Create_Child_Scanner
     (Parent : DOM.Core.Node)
      return XML_Scanner
   is
      use Ada.Finalization;
   begin
      return XML_Scanner'(Limited_Controlled with Acc =>
                             new XML_Scanner_Data'
                            (Nodes  => DOM.Core.Nodes.Child_Nodes (Parent),
                             Cursor => 0));
   end Create_Child_Scanner;

   ----------------------
   -- No_More_Children --
   ----------------------

   function No_More_Children
     (Scanner : XML_Scanner)
      return Boolean
   is
   begin
      return Scanner.Acc.Cursor >= DOM.Core.Nodes.Length (Scanner.Acc.Nodes);
   end No_More_Children;

   function Remaining_Children (Scanner : XML_Scanner)
                                return Natural
   is
   begin
      if Scanner.No_More_Children then
         return 0;
      else
         return DOM.Core.Nodes.Length (Scanner.Acc.Nodes)-Scanner.Acc.Cursor;
      end if;
   end Remaining_Children;

   ---------------
   -- Peek_Name --
   ---------------

   function Peek_Name
     (Scanner : XML_Scanner)
      return String
   is
      use DOM.Core.Nodes;
   begin
      if Scanner.No_More_Children then
         return No_Name;
      else
         return Node_Name (Item (Scanner.Acc.Nodes, Scanner.Acc.Cursor));
      end if;
   end Peek_Name;

   function Peek_Node
     (Scanner : XML_Scanner)
      return DOM.Core.Node
   is
      use DOM.Core.Nodes;
   begin
      if Scanner.No_More_Children then
         return null;
      else
         return Item (Scanner.Acc.Nodes, Scanner.Acc.Cursor);
      end if;
   end Peek_Node;

   ----------
   -- Scan --
   ----------

   function Scan
     (Scanner : XML_Scanner)
      return DOM.Core.Node
   is
      use DOM.Core;
   begin
      if Scanner.No_More_Children then
         return null;
      else
         declare
            Result : constant Node := Scanner.Peek_Node;
         begin
            Scanner.Acc.Cursor := Scanner.Acc.Cursor + 1;
            return Result;
         end;
      end if;
   end Scan;

   procedure Expect (Scanner : XML_Scanner;
                     Name    : String)
   is
   pragma Unreferenced (Name);
   begin
      if Scanner.Peek_Name /= "name" then
         raise Unexpected_Node;
      end if;
   end Expect;

   procedure Parse_Sequence
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Callback   : not null access procedure (N : DOM.Core.Node);
      Min_Length : in     Natural := 1;
      Max_Length : in     Natural := No_Limit)
   is
      Counter : Natural := 0;
   begin
      Put_Line ("PARSE SEQUENCE");
      Dom.Core.Nodes.Print (Scanner.Peek_Node);

      while Scanner.Peek_Name = Name loop
         if Max_Length /= No_Limit and Counter = Max_Length then
            raise Unexpected_Node;
         end if;

         Callback (Scanner.Scan);
         Counter := Counter + 1;
      end loop;

      if Counter < Min_Length then
         Put("[");
         Dom.Core.Nodes.Print (Scanner.Peek_Node);
         Put_Line ("]");
         raise Unexpected_Node with "Expected '" & Name & "' found '" & Scanner.Peek_Name & "'" ;
      end if;
   end Parse_Sequence;

   procedure Parse_Optional
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Callback   : not null access procedure (N : DOM.Core.Node))
   is
   begin
      Scanner.Parse_Sequence (Name       => Name,
                              Callback   => Callback,
                              Min_Length => 0,
                              Max_Length => 1);
   end Parse_Optional;

   procedure Parse_Single_Node
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Callback   : not null access procedure (N : DOM.Core.Node))
   is
   begin
      Scanner.Parse_Sequence (Name       => Name,
                              Callback   => Callback,
                              Min_Length => 1,
                              Max_Length => 1);
   end Parse_Single_Node;

   procedure Expect_End_Of_Children (Scanner : XML_Scanner)
   is
   begin
      if not Scanner.No_More_Children then
         raise Unexpected_Node;
      end if;
   end Expect_End_Of_Children;

   function Parse_Text_Node (Scanner : XML_Scanner;
                             Name    : String)
                             return String
   is
      use DOM.Core;
   begin
      if Scanner.Peek_Name /= Name then
         raise Unexpected_Node;
      end if;

      declare
         S : constant XML_Scanner := Create_Child_Scanner(Scanner.Scan);
      begin
         if S.Remaining_Children /= 1 or else S.Peek_Name /= "#text" then
            raise Unexpected_Node;
         end if;

         return Nodes.Node_Value (S.Scan);
      end;
   end Parse_Text_Node;

   procedure Process_Sequence
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Processor  : in out Abstract_Node_Processor'Class;
      Min_Length : in     Natural := 1;
      Max_Length : in     Natural := No_Limit)
   is
      procedure Call_Processor (N : DOM.Core.Node) is
      begin
         Processor.Process (N);
      end Call_Processor;
   begin
      Scanner.Parse_Sequence (Name       => Name,
                              Callback   => Call_Processor'Access,
                              Min_Length => Min_Length,
                              Max_Length => Max_Length);
   end Process_Sequence;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Object : in out XML_Scanner)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => XML_Scanner_Data,
                                        Name   => XML_Scanner_Data_Access);
   begin
      Free (Object.Acc);
   end Finalize;

end XML_Scanners;
