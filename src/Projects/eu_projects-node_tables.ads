with Ada.Containers.Ordered_Maps;

with EU_Projects.Nodes;

package EU_Projects.Node_Tables is
   type Node_Table is tagged private
     with Constant_Indexing => Element;

   type Table_Ref (D : access Node_Table) is tagged limited  null record
     with Implicit_Dereference => D;

   function Contains (T  : Node_Table;
                      ID : Nodes.Node_Label) return Boolean;

   function Element (T  : Node_Table;
                     ID : Nodes.Node_Label) return Nodes.Node_Access
     with
       Pre => T.Contains (ID);

   procedure Insert (T    : in out Node_Table;
                     ID   : Nodes.Node_Label;
                     Item : Nodes.Node_Access)
     with
       Pre => not T.Contains (ID),
     Post => T.Contains (ID);


   function Labels_Of (T     : Node_Table;
                       Class : Nodes.Node_Class)
                       return Nodes.Node_Label_Lists.Vector;

   procedure Dump (T : Node_Table);

   Duplicated_Label : exception;
private
   use type Nodes.Node_Label;
   use type Nodes.Node_Access;

   package Node_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Nodes.Node_Label,
                                      Element_Type => Nodes.Node_Access);

   type Node_Table is tagged
      record
         Table : Node_Maps.Map;
      end record;

   function Contains (T  : Node_Table;
                      ID : Nodes.Node_Label) return Boolean
   is (T.Table.Contains (ID));

   function Element (T  : Node_Table;
                     ID : Nodes.Node_Label) return Nodes.Node_Access
   is (T.Table.Element (ID));

end EU_Projects.Node_Tables;
