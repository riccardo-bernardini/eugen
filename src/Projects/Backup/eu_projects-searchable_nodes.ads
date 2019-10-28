with EU_Projects.Nodes;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package EU_Projects.Searchable_Nodes is
   type Child_Class is (Void, Node, Attribute);

   type Child_Value (Class : Child_Class) is
      record
         case Class is
            when Void =>
               null;

            when Node  =>
               Child : Nodes.Node_Access;

            when Attribute =>
               Name  : Unbounded_String;
               Value : Unbounded_String;
         end case;
      end record;

   No_Child : constant Child_Value := (Class => Void);

   type Searchable_Node is limited interface;


   function Find (Where : Searchable_Node;
                  Label : Identifier)
                  return Child_Value
                  is abstract;
   --  Search for a child with the given name and return it
   --  Return No_Child if the child does not exist

   function Exists (Where : Searchable_Node;
                    Label : Identifiers.Identifier)
                    return Child_Class
                    is abstract;

   Duplicated_Name : exception;
end EU_Projects.Searchable_Nodes;
