package body EU_Projects.Nodes.Timed_Nodes is

   procedure Due_On (Item : in out Timed_Node;
                     Time : in     String)
   is
   begin
      Item.Expected_Raw := To_Unbounded_String (Time);
      --          Times.Time_Expressions.Symbolic (Time);
   end Due_On;

   procedure Parse_Raw_Expressions (Item : in out Timed_Node;
                                    Vars : Times.Time_Expressions.Parsing.Symbol_Table)
   is
      use Times.Time_Expressions.Parsing;

      To_Parse : constant String :=
                   (if Item.Expected_Raw = Event_Names.Default_Time
                    then
                       After (Timed_Node'Class (Item).Dependency_List,
                      Timed_Node'Class (Item).Dependency_Ready_Var)
                    else
                       To_String (Item.Expected_Raw));
   begin
      Item.Expected_Symbolic := Parse (Input   => To_Parse,
                                       Symbols => Vars);
   end Parse_Raw_Expressions;


   overriding procedure Fix_Instant
     (Item  : in out Timed_Node;
      Var   : Simple_Identifier;
      Value : Times.Instant)
   is
   begin
      if Var = Event_Names.Event_Time_Name then
         Item.Expected_On := Value;
      else
         raise Unknown_Instant_Var with To_String (Var);
      end if;

      Item.Fixed := True;
   end Fix_Instant;


   --------------
   -- Is_Fixed --
   --------------

   overriding function Is_Fixed (Item : Timed_Node;
                                 Var  : Simple_Identifier)
                                 return Boolean
   is
   begin
      if Var = Event_Names.Event_Time_Name then
         return Item.Fixed;
      else
         raise Unknown_Var with To_String (Var);
      end if;
   end Is_Fixed;


end EU_Projects.Nodes.Timed_Nodes;
