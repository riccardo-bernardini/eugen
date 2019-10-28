with DOM.Core;

with Ada.Finalization;

package XML_Scanners is
   --
   -- As well knnown, an XML document is a tree.  An XML_Scanner is
   -- an object associated to a tree of the node that allows to
   -- iterate over the node children.  At every time there is a
   -- "current child." The function Scan returns the current child
   -- and "consumes" it.
   --
   type XML_Scanner (<>) is
     new Ada.Finalization.Limited_Controlled
   with
     private;

   function Create_Child_Scanner (Parent : DOM.Core.Node) return XML_Scanner;

   -- Return true if all the children have been used
   function No_More_Children (Scanner : XML_Scanner) return Boolean;

   -- Return the number of children still to be used
   function Remaining_Children (Scanner : XML_Scanner) return Natural;

   -- Return the current node, but do not consume it
   function Peek_Node (Scanner : XML_Scanner) return DOM.Core.Node;

   -- Raise Unexpected_Node unless the current node has the given name.
   -- Do not consume the node.
   procedure Expect (Scanner : XML_Scanner;  Name : String);

   -- Raise Unexpected_Node unless all the children have been used.
   procedure Expect_End_Of_Children (Scanner : XML_Scanner);

   --
   -- Expect that current node has the specified name and it is a
   -- "pure text" node, that is, a node whose only children is text.
   -- If everything is OK, it consumes the node and returns the node content,
   -- otherwise raises Unexpected_Node.
   --
   function Parse_Text_Node (Scanner : XML_Scanner;
                             Name    : String)
                             return String;

   No_Limit : constant Natural;

   --
   -- Expect a sequence of nodes with the given name.  For every node
   -- calls the given callback.  Unexpected_Node is raised if the
   -- number of nodes is not within the specified limits
   --
   procedure Parse_Sequence
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Callback   : not null access procedure (N : DOM.Core.Node);
      Min_Length : in     Natural := 1;
      Max_Length : in     Natural := No_Limit);

   -- Like Parse_Sequence with min=0, max=1
   procedure Parse_Optional
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Callback   : not null access procedure (N : DOM.Core.Node));

   -- Like Parse_Sequence with min=max=1
   procedure Parse_Single_Node
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Callback   : not null access procedure (N : DOM.Core.Node));


   No_Name : constant String;

   function Peek_Name (Scanner : XML_Scanner)
                  return String;

   function Scan (Scanner : XML_Scanner)
                  return DOM.Core.Node;

   Unexpected_Node : exception;

   type Abstract_Node_Processor is interface;

   procedure Process (Processor : in out Abstract_Node_Processor;
                      Node      : in     DOM.Core.Node)
   is abstract;

   procedure Process_Sequence
     (Scanner    : in out XML_Scanner;
      Name       : in     String;
      Processor  : in out Abstract_Node_Processor'Class;
      Min_Length : in     Natural := 1;
      Max_Length : in     Natural := No_Limit);

private
   No_Name : constant String := "";
   No_Limit : constant Natural := Natural'Last;

   type XML_Scanner_Data is
      record
         Nodes  : DOM.Core.Node_List;
         Cursor : Natural;
      end record;

   type XML_Scanner_Data_Access is access XML_Scanner_Data;

   type XML_Scanner is
     new Ada.Finalization.Limited_Controlled with
      record
         Acc : XML_Scanner_Data_Access;
      end record;

   overriding
   procedure Finalize (Object : in out XML_Scanner);
end XML_Scanners;
