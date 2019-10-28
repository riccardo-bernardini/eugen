with EU_Projects.Nodes.Partners;
with EU_Projects.Times.Time_Expressions;
with EU_Projects.Event_Names;
with EU_Projects.Node_Tables;
with EU_Projects.Efforts;

--
-- An action node is a basic node that represent an activity, that is, a WP
-- or a task.  An action has
--
--   * A leader
--   * An activity interval with begin and end dates
--
-- Actually, there is the necessity of two types of begin/end dates.  At
-- the beginning the activity dates can be expressed in symbolic, e.g.,
-- the begin date of a task is the largest of the end dates of two
-- other tasks.
--
-- The constraints expressed in this way will be mapped to absolute
-- dates by solving the corrispondent system of equations. After that, the
-- symbolic dates need to be changed to their "absolute" counterpart.  The
-- symbolic dates are set during creation time, while the absolute times need
-- to be fixed after all the project has been set.
--
-- Because of this, an Action_Node has two states: if the absolute
-- action times have been fixed or not.  Absolute times can be fixed
-- only once.
--
package EU_Projects.Nodes.Action_Nodes is
   type Action_Node is abstract new Node_Type with private;

   type Instant_Raw is new String;

   type Duration_Raw is new String;

   type Action_Time is private;

   function Create (Start_On : Instant_Raw;
                    End_On   : Instant_Raw)
                    return Action_Time;

   function Create (Depends_On : Node_Label_Lists.Vector;
                    End_On     : Instant_Raw)
                    return Action_Time;


   function Create (Start_On : Instant_Raw;
                    Duration : Duration_Raw)
                    return Action_Time;

   function Create (Depends_On : Node_Label_Lists.Vector;
                    Duration   : Duration_Raw)
                    return Action_Time;


   function Starting_Time (Item : Action_Node) return Times.Instant
     with Pre => Item.Is_Fixed (Event_Names.Begin_Name);

   function Ending_Time (Item : Action_Node) return Times.Instant
     with Pre => Item.Is_Fixed (Event_Names.End_Name);

   function Timing (Item : Action_Node) return String;


   function Leader (Item : Action_Node) return Partners.Partner_Label;

   overriding function Variables (Item : Action_Node) return Variable_List;

   overriding procedure Parse_Raw_Expressions
     (Item : in out Action_Node;
      Vars : Times.Time_Expressions.Parsing.Symbol_Table);

   overriding function Is_Variable (Item : Action_Node;
                                    Var  : Simple_Identifier) return Boolean;

   overriding function Is_A (Item  : Action_Node;
                             Var   : Simple_Identifier;
                             Class : Times.Time_Type)
                             return Boolean;

   overriding function Is_Fixed (Item : Action_Node;
                                 Var  : Simple_Identifier)
                                 return Boolean;


   overriding procedure Fix_Instant
     (Item  : in out Action_Node;
      Var   : Simple_Identifier;
      Value : Times.Instant);

   overriding function Get_Symbolic_Instant
     (Item : Action_Node;
      Var  : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Instant;

   overriding function Get_Symbolic_Duration
     (Item   : Action_Node;
      Var    : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Duration;

   procedure Add_Effort (Item    : in out Action_Node;
                         Partner : in Nodes.Partners.Partner_Label;
                         PM      : in Efforts.Person_Months);

   function Effort_Of (Item    : Action_Node;
                       Partner : Nodes.Partners.Partner_Label)
                       return Efforts.Person_Months;

   type Effort_Entry is
      record
         Partner : Nodes.Partners.Partner_Label;
         Effort  : Efforts.Person_Months;
      end record;

   type Effort_List is array (Partners.Partner_Index range <>) of Effort_Entry;

   function Efforts_Of (Item  : Action_Node;
                        Names : Partners.Partner_Name_Array) return Effort_List;

   Duplicated_Effort : exception;
   Unknown_Partner   : exception;


private
   use  type Times.Time_Expressions.Symbolic_Instant;
   use  type Times.Time_Expressions.Symbolic_Duration;
   use  type Times.Time_Type;


   type Action_Time is
      record
         Start    : Unbounded_String;
         Stop     : Unbounded_String;
         Duration : Unbounded_String;
      end record
     with Type_Invariant => (if Stop = Null_Unbounded_String
                               then Duration /= Null_Unbounded_String
                                 else Duration = Null_Unbounded_String);

   function Var_Begin (Item : Action_Node) return Times.Time_Expressions.Symbolic_Instant;
   function Var_End (Item : Action_Node) return Times.Time_Expressions.Symbolic_Instant;
   function Var_Duration (Item : Action_Node) return Times.Time_Expressions.Symbolic_Duration;

   procedure Initialize_Efforts (Item     : in out Action_Node;
                                 Node_Dir : Node_Tables.Node_Table);


   type Action_Node is abstract
   new Node_Type
   with
      record
         Raw_Time : Action_Time;

         Start_Symbolic    : Times.Time_Expressions.Symbolic_Instant;
         Stop_Symbolic     : Times.Time_Expressions.Symbolic_Instant;
         Duration_Symbolic : Times.Time_Expressions.Symbolic_Duration;

         Start_At     : Times.Instant;
         Stop_At      : Times.Instant;
         Elapsed_Time : Times.Duration;

         Start_Fixed   : Boolean := False;
         Stop_Fixed    : Boolean := False;

         Leader        : Nodes.Partners.Partner_Label;

         Partner_Effort : Efforts.Effort_Maps.Map;
      end record;

   function To_U (X : Instant_Raw) return Unbounded_String
   is (To_Unbounded_String (String (X)));

   function To_U (X : Duration_Raw) return Unbounded_String
   is (To_Unbounded_String (String (X)));

   function After (List : Node_Label_Lists.Vector)
                   return Instant_Raw;

   function Create (Start_On : Instant_Raw;
                    End_On   : Instant_Raw)
                    return Action_Time
   is (Start => To_U (Start_On), Stop => To_U (End_On), Duration => Null_Unbounded_String);

   function Create (Depends_On : Node_Label_Lists.Vector;
                    End_On     : Instant_Raw)
                    return Action_Time
   is (Create (After (Depends_On), End_On));


   function Create (Start_On : Instant_Raw;
                    Duration : Duration_Raw)
                    return Action_Time
   is (Start => To_U (Start_On), Stop => Null_Unbounded_String, Duration => To_U (Duration));


   function Create (Depends_On : Node_Label_Lists.Vector;
                    Duration   : Duration_Raw)
                    return Action_Time
   is (Create (After (Depends_On), Duration));


   function Leader (Item : Action_Node) return Partners.Partner_Label
   is (Item.Leader);


   function Starting_Time (Item : Action_Node) return Times.Instant
   is (Item.Start_At);

   function Ending_Time (Item : Action_Node) return Times.Instant
   is (Item.Stop_At);


   function Timing (Item : Action_Node) return String
   is ("M" & Times.Image (Item.Starting_Time) & "-M" & Times.Image (Item.Ending_Time));

   --     function Event_Var (Item : Action_Node;
   --                         Event : Event_Type) return Identifier
   --     is (case Event is
   --            when Start => Item.Starting_Time_Var,
   --            when Stop  => Item.Ending_Time_Var);
   --
   --     function Event_Time (Item  : Action_Node;
   --                          Event : Event_Type) return Times.Instant
   --     is (case Event is
   --            when Start => Item.Starting_Time,
   --            when Stop  => Item.Ending_Time);


   function Variables (Item : Action_Node) return Variable_List
   is ((1 => Event_Names.Begin_Name,
        2 => Event_Names.End_Name,
        3 => Event_Names.Duration_Name));

   function Is_Variable (Item : Action_Node;
                         Var  : Simple_Identifier) return Boolean
   is (Var = Event_Names.Begin_Name
       or Var = Event_Names.End_Name
       or Var = Event_Names.Duration_Name);

   function Is_A (Item  : Action_Node;
                  Var   : Simple_Identifier;
                  Class : Times.Time_Type)
                  return Boolean
   is (if Var = Event_Names.Begin_Name or Var = Event_Names.End_Name then
          Class = Times.Instant_Value

       elsif Var = Event_Names.Duration_Name then
          Class = Times.Duration_Value

       else
          False);

   function Is_Fixed (Item : Action_Node;
                      Var  : Simple_Identifier)
                      return Boolean
   is (if Var = Event_Names.Begin_Name then
          Item.Start_Fixed

       elsif Var = Event_Names.End_Name then
          Item.Stop_Fixed

       elsif Var = Event_Names.Duration_Name then
          Item.Start_Fixed and Item.Stop_Fixed

       else
          raise Constraint_Error);


   overriding function Get_Symbolic_Instant
     (Item   : Action_Node;
      Var    : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Instant
   is (if Var = Event_Names.Begin_Name then
          Item.Start_Symbolic

       elsif Var = Event_Names.End_Name then
          Item.Stop_Symbolic

       else
          raise Unknown_Instant_Var);


   overriding function Get_Symbolic_Duration
     (Item   : Action_Node;
      Var    : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Duration
   is (if Var = Event_Names.Duration_Name then
          Item.Duration_Symbolic

       else
          raise Unknown_Duration_Var);

 function Effort_Of (Item    : Action_Node;
                       Partner : Nodes.Partners.Partner_Label)
                       return Efforts.Person_Months
   is (if Item.Partner_Effort.Contains (Partner) then
          Item.Partner_Effort (Partner)
       else
          raise Unknown_Partner);


end EU_Projects.Nodes.Action_Nodes;
