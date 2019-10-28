with EU_Projects.Times.Time_Expressions;
with EU_Projects.Event_Names;

--
-- A timed node represents something related with an event.  It is a
-- basic node with a "expected on" date.  Currently used for deliverables
-- and milestones
--
package EU_Projects.Nodes.Timed_Nodes is
   type Timed_Node is abstract new Node_Type with private;

   type Timed_Node_Access is access all Timed_Node'Class;

   --     function Create (Label       : Identifiers.Identifier;
   --                      Name        : String;
   --                      Short_Name  : String;
   --                      Index       : Node_Index := No_Index)
   --                      return Timed_Node'Class;

   Node_Event_Name : constant Dotted_Identifier := To_ID ("expected_on");

   function Time_Fixed (Item : Timed_Node) return Boolean;

   procedure Due_On (Item : in out Timed_Node;
                     Time : in     String);

   function Due_On (Item : Timed_Node) return Times.Instant
     with Pre => Item.Time_Fixed;


   function Due_On (Item : Timed_Node)
                    return Times.Time_Expressions.Symbolic_Instant;

   function Due_Time_Var (Item : Timed_Node) return Dotted_Identifier;

   function Dependency_Ready_Var (Item : Timed_Node) return String
                                  is abstract;

   overriding function Variables (Item : Timed_Node) return Variable_List
   is ((1 => Event_Names.Event_Time_Name));

   overriding procedure Parse_Raw_Expressions
     (Item : in out Timed_Node;
      Vars : Times.Time_Expressions.Parsing.Symbol_Table);

   overriding function Is_Variable (Item : Timed_Node;
                                    Var  : Simple_Identifier) return Boolean;

   overriding function Is_A (Item  : Timed_Node;
                             Var   : Simple_Identifier;
                             Class : Times.Time_Type)
                             return Boolean;

   overriding function Is_Fixed (Item : Timed_Node;
                                 Var  : Simple_Identifier)
                                 return Boolean;


   overriding procedure Fix_Instant
     (Item  : in out Timed_Node;
      Var   : Simple_Identifier;
      Value : Times.Instant)
     with
       Pre => not Item.Time_Fixed,
       Post => Item.Time_Fixed;

   overriding function Get_Symbolic_Instant
     (X   : Timed_Node;
      Var : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Instant;

   overriding function Get_Symbolic_Duration
     (X   : Timed_Node;
      Var : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Duration;
private
   use type Times.Time_Type;

   type Timed_Node is abstract
   new Node_Type
   with
      record
         Expected_Raw      : Unbounded_String;
         Expected_Symbolic : Times.Time_Expressions.Symbolic_Instant;
         Expected_On       : Times.Instant;
         Fixed             : Boolean := False;
      end record;

   function Time_Fixed (Item : Timed_Node) return Boolean
   is (Item.Fixed);

   function Due_On (Item : Timed_Node) return Times.Time_Expressions.Symbolic_Instant
   is (Item.Expected_Symbolic);

   function Due_On (Item : Timed_Node) return Times.Instant
   is (Item.Expected_On);

   function Due_Time_Var (Item : Timed_Node) return Dotted_Identifier
   is (Join (Dotted_Identifier (Item.Label), Node_Event_Name));

   function Is_Variable (Item : Timed_Node;
                         Var  : Simple_Identifier) return Boolean
   is (Var = Event_Names.Event_Time_Name);

   overriding function Is_A (Item  : Timed_Node;
                             Var   : Simple_Identifier;
                             Class : Times.Time_Type)
                             return Boolean
   is (Var = Event_Names.Event_Time_Name and Class = Times.Instant_Value);


   function Get_Symbolic_Instant
     (X   : Timed_Node;
      Var : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Instant
   is (if Var = Event_Names.Event_Time_Name then
          X.Expected_Symbolic
       else
          raise Unknown_Instant_Var);

   function Get_Symbolic_Duration
     (X   : Timed_Node;
      Var : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Duration
   is (raise Unknown_Duration_Var);
end EU_Projects.Nodes.Timed_Nodes;
