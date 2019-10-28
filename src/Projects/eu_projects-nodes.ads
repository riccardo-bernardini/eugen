with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Finalization;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with EU_Projects.Times.Time_Expressions.Parsing;

-- Most of the entities in a project (WPs, tasks, deliverables, ...) share
-- many characteristics such as a name, a label (an ID, in XML jargon),
-- a short name, ... Moreover, there are some entities such as WPs and
-- tasks that have bengin/end dates while others (deliverables and milestones)
-- have just an "expected" date.
--
-- It is worth to "factorize" all these properties in a hierachy of classes.
-- More precisely we will have
--
--    * Basic nodes that have
--        - a name
--        - a short name
--        - a label
--        - an index
--        - a description
--    * Action nodes that are basic nodes with begin/end dates
--    * Timed nodes that are basic nodes with an "expected on" date
--
--
-- About names: the name of a node has possibly three parts
--
--   * A prefix that specify the type of node (T, D, ...)
--   * An index that can be simple (e.g., 4) or composite (e.g., 4.3)
--   * A title.  Should we allow long+short?
--
-- A node can also have some "free attributes."  This can be useful in some
-- context where I want to store some information about WP, task, ...
-- that was not forecasted.
--
package EU_Projects.Nodes is
    type Node_Label is new Dotted_Identifier;

   package Node_Label_Lists is
     new Ada.Containers.Vectors (Positive, Node_Label);

   function Parse_Label_List (Input : String) return Node_Label_Lists.Vector;

   function Join (List      : Node_Label_Lists.Vector;
                  Separator : String)
                  return String;

   function After (Labels   : Node_Label_Lists.Vector;
                   Done_Var : String) return String;

   type Node_Class is (Info_Node, WP_Node, Task_Node, Deliverable_Node, Milestone_Node, Risk_Node, Partner_Node);

   type Node_Index is new Positive;

   subtype Extended_Node_Index is
     Node_Index'Base range Node_Index'First - 1 .. Node_Index'Last;

   No_Index : constant Extended_Node_Index := Extended_Node_Index'First;

   function Image (X : Node_Index) return String
   is (Chop (Node_Index'Image (X)));

   -- Many nodes have an "add" function that allow to add new
   -- children to a node (e.g. tasks to a WP).  Every node has an
   -- index.  The most natural place where to add the index is
   -- inside the parent, since the parent knows the number of
   -- its children.  However, it is possible that in some cases
   -- one could want to set indixes in a non-incremental way.
   -- Therefore, if the index has already been set, it must not be
   -- changed.
   --
   -- The following function can be used in the postcondition
   -- of an "add" function.

   function Correctly_Updated (Old_Index, New_Index : Extended_Node_Index;
                               Old_Max, New_Max     : Extended_Node_Index)
                               return Boolean
   is (
       (
        (Old_Index = No_Index and New_Index /= No_Index)
        or (Old_Index = New_Index)
       )
       and (
            (Old_Index /= No_Index)
            or (New_Index = Old_Max + 1)
           )
       and (New_Max = Node_Index'Max (Old_Max, New_Index))
      );

   type Node_Type (<>) is abstract new Ada.Finalization.Controlled with private;

   type Node_Access is  access all Node_Type'Class;


   --     function Create (Label       : Identifiers.Identifier;
   --                      Name        : String;
   --                      Short_Name  : String;
   --                      Description : String;
   --                      Index       : Node_Index := No_Index)
   --                      return Node_Type;

   function Index (Item : Node_Type) return Extended_Node_Index;

   function Index_Image (Item : Node_Type) return String;

   function Full_Index (Item     : Node_Type;
                        Prefixed : Boolean) return String
                        is abstract;

   --     procedure Set_Index (Item : in out Node_Type;
   --                          Idx  : Node_Index)
   --       with
   --         Pre => Item.Index = No_Index;

   procedure Add_Attribute (Item  : in out Node_Type;
                            Name  : in    String;
                            Value : in    String);


   function Name (Item : Node_Type)
                  return String;

   function Short_Name (Item : Node_Type)
                        return String;

   function Label (Item : Node_Type)
                   return Node_Label;

   function Description (Item : Node_Type) return String;

   function Attribute_Exists (Item : Node_Type;
                              Name : String)
                              return Boolean;

   function Get_Attribute (Item : Node_Type;
                           Name : String)
                           return String;

   function Get_Attribute (Item    : Node_Type;
                           Name    : String;
                           Default : String)
                           return String;

   Bad_Parent : exception;

   function Class (Item : Node_Type) return Node_Class;

   type Variable_List is array (Positive range<>) of Simple_Identifier;
   Empty_List : constant Variable_List (2 .. 1) := (others => <>);

   function Variables (Item : Node_Type) return Variable_List
   is (Empty_List);
   -- Every node has a number of internal variables.  For example, a WP
   -- has a starting time, and ending time and a duration (never mind that
   -- they are not independent, they are three different variables for
   -- what matters here).  This function returns a list of the internal
   -- variables.  Note that they are Simple_Identifiers, therefore they
   -- have no '.'
   --
   -- For example, a WP would return "begin", "end", "duration".

   procedure Parse_Raw_Expressions
     (Item : in out Node_Type;
      Vars : Times.Time_Expressions.Parsing.Symbol_Table)
   is null;
   -- While parsing the nodes are initialized with time expressions in
   -- string form and we need to parse them before we try to solve
   -- the corresponding system of equations.
   --
   -- We need to do this in two separate steps since some "forward reference"
   -- can be needed.  For example, the ending time of a WP can have the special
   -- value "end" that means when the last task ot the WP ends.  In order to
   -- handle this the WP needs to know all its tasks.

   function Dependency_List (Item : Node_Type)
                             return Node_Label_Lists.Vector
                             is abstract;
   --
   -- A start time/due time has as default value the maximum of the end
   -- times of all the "dependencies."  This function returns the list
   -- of the labels of all dependencies.
   --

   pragma Warnings (Off);

   function Is_Variable (Item : Node_Type;
                         Var  : Simple_Identifier) return Boolean
   is (False);
   -- Return if Var is a variable known to the node.  Useful in contracts

   function Is_A (Item  : Node_Type;
                  Var   : Simple_Identifier;
                  Class : Times.Time_Type)
                  return Boolean
   is (False);
   --  Return True if Var is a variable known to the node AND it contains
   --  value of the specified type.

   function Is_Fixed (Item : Node_Type;
                      Var  : Simple_Identifier)
                      return Boolean
     with Pre'Class => Is_Variable (Item, Var);

   pragma Warnings (On);

   function Get_Symbolic_Instant (Item : Node_Type;
                                  Var  : Simple_Identifier)
                                  return Times.Time_Expressions.Symbolic_Instant
                                  is abstract;

   function Get_Symbolic_Duration (Item : Node_Type;
                                   Var  : Simple_Identifier)
                                   return Times.Time_Expressions.Symbolic_Duration
                                   is abstract;

   procedure Fix_Instant
     (Item  : in out Node_Type;
      Var   : Simple_Identifier;
      Value : Times.Instant)
     with
       Pre'Class => Item.Is_A (Var, Times.Instant_Value) and then not Item.Is_Fixed (Var),
     Post'Class => Item.Is_Fixed (Var);


   --     procedure Fix_Duration
   --       (Item  : in out Node_Type;
   --        Var   : Simple_Identifier;
   --        Value : Times.Duration)
   --       with
   --         Pre'Class => Item.Is_A (Var, Times.Duration_Value) and then not Item.Is_Fixed (Var),
   --       Post'Class => Item.Is_Fixed (Var);


   Unknown_Var          : exception;
   Unknown_Instant_Var  : exception;
   Unknown_Duration_Var : exception;
   --     --
   --     -- There is the necessity of writing pre/post conditions that
   --     -- check that a Node_Access points to a WP, Task, ... Because of
   --     -- mutual dependences (e.g., a WP has a list of tasks and every
   --     -- task has a "Parent" field pointing back to the WP), it is
   --     -- not easy to use the facilities of Ada.Tags sinche it would
   --     -- introduce circular dependencies.  By using function Is_A defined
   --     -- here I can break those dependencies.
   --     --
   --     type Node_Class is (A_WP, A_Task, A_Deliverable, A_Milestone, A_Partner);

   --     function Is_A (Item : Node_Type'Class;
   --                    Class : Node_Class)
   --                    return Boolean;
private
   function Shortify (Short_Name, Name : String) return Unbounded_String
   is (
       (if Short_Name = "" then
           To_Unbounded_String (Name)
        else
           To_Unbounded_String (Short_Name)
       )
      );


   package Attribute_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => String);


   type Node_Type is abstract
   new Ada.Finalization.Controlled
   with
      record
         Class       : Node_Class;
         Label       : Node_Label;
         Name        : Unbounded_String;
         Short_Name  : Unbounded_String;
         Index       : Extended_Node_Index := No_Index;
         Description : Unbounded_String;
         Attributes  : Attribute_Maps.Map;
         --           Parent      : Node_Access;
      end record;

   function Trim (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Index_Image (Item : Node_Type) return String
   is (Trim (Extended_Node_Index'Image (Item.Index)));

   function Index (Item : Node_Type) return Extended_Node_Index
   is (Item.Index);

   function Class (Item : Node_Type) return Node_Class
   is (Item.Class);

   --     function Full_Index (Item : Node_Type) return String
   --     is (
   --         if Item.Parent = null then
   --            Trim (Node_Index'Image (Item.Index))
   --         else
   --            Trim (Node_Index'Image (Item.Parent.Index))
   --         & "."
   --         & Trim (Node_Index'Image (Item.Index))
   --        );

   --     function Index (Item : Node_Type) return String
   --     is (Trim (Node_Index'Image (Item.Index)));

   function Name (Item : Node_Type) return String
   is (To_String (Item.Name));

   function Short_Name (Item : Node_Type) return String
   is (To_String (Item.Short_Name));

   function Label (Item : Node_Type) return Node_Label
   is (Item.Label);

   function Description (Item : Node_Type) return String
   is (To_String (Item.Description));


   function Attribute_Exists (Item : Node_Type;
                              Name : String)
                              return Boolean
   is (Item.Attributes.Contains (Name));

   function Get_Attribute (Item : Node_Type;
                           Name : String)
                           return String
   is (Item.Attributes.Element (Name));

   function Get_Attribute (Item    : Node_Type;
                           Name    : String;
                           Default : String)
                           return String
   is (
       if Item.Attribute_Exists (Name) then
          Item.Attributes.Element (Name)
       else
          Default
      );

   -- Convenience function: this action is quite common in many
   -- node instances and we "factor" it here
   function Make_Short_Name (Short_Name : String;
                             Default    : String)
                             return Unbounded_String
   is (
       if Short_Name = "" then
          To_Unbounded_String (Default)
       else
          To_Unbounded_String (Short_Name)
      );

end EU_Projects.Nodes;
