with DOM.Core;

package XML_Utilities is
   function Parse_String (Item : String) return DOM.Core.Document;

   --
   -- If node N has the given attribute, return its value otherwise raise
   -- No_such_Attribute
   --
   function Expect_Attribute (N : DOM.Core.Node; Name : String) return String;

   --
   -- Return True if the node has the given attribute
   --
   function Has_Attribute (N : DOM.Core.Node; Name : String) return Boolean;

   --
   -- If node N has the given attribute, return its value otherwise return
   -- the default value
   --
   function Get_Attribute (N       : DOM.Core.Node;
                           Name    : String;
                           Default : String := "")
                           return String;

   No_Such_Attribute : exception;
end XML_Utilities;
