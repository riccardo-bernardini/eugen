--  with Symbolic_Identifiers;

package body EU_Projects.Nodes.Action_Nodes is
   use Times.Time_Expressions;

   function Var_Begin (Item : Action_Node) return Symbolic_Instant
   is (Variable (Join (Dotted_Identifier (Item.Label), Event_Names.Begin_Name)));

   function Var_End (Item : Action_Node) return Symbolic_Instant
   is (Variable (Join (Dotted_Identifier (Item.Label), Event_Names.End_Name)));

   function Var_Duration (Item : Action_Node) return Symbolic_Duration
   is (Variable (Join (Dotted_Identifier (Item.Label), Event_Names.Duration_Name)));

----------------
-- Efforts_Of --
----------------

   function Efforts_Of (Item  : Action_Node;
                        Names : Partners.Partner_Name_Array) return Effort_List
   is
      Result : Effort_List (Names'Range);
   begin
      for Idx in Names'Range loop
         Result (Idx) := Effort_Entry'(Partner => Names (Idx),
                                       Effort  => Item.Partner_Effort(Names(Idx)));
      end loop;

      return Result;
   end Efforts_Of;

   ---------------------------
   -- Parse_Raw_Expressions --
   ---------------------------

   overriding procedure Parse_Raw_Expressions
     (Item : in out Action_Node;
      Vars : Times.Time_Expressions.Parsing.Symbol_Table)
   is
      use Times.Time_Expressions.Parsing;
   begin
      Item.Start_Symbolic := Parse (To_String (Item.Raw_Time.Start), Vars);

      if Item.Raw_Time.Stop = Null_Unbounded_String then
         Item.Stop_Symbolic := Item.Var_Begin + Item.Var_Duration;
         Item.Duration_Symbolic := Parse (To_String (Item.Raw_Time.Duration), Vars);

      else
         Item.Stop_Symbolic := Parse (To_String (Item.Raw_Time.Stop), Vars);
         Item.Duration_Symbolic := Item.Var_End - Item.Var_Begin;
      end if;
   end Parse_Raw_Expressions;

   -----------------
   -- Fix_Instant --
   -----------------

   overriding procedure Fix_Instant
     (Item  : in out Action_Node;
      Var   : Simple_Identifier;
      Value : Times.Instant)
   is
      use Times;
   begin
      if Var = Event_Names.Begin_Name then
         Item.Start_At := Value;
         Item.Start_Fixed := True;

      elsif Var = Event_Names.End_Name then
         Item.Stop_At := Value;
         Item.Stop_Fixed := True;

      else
         raise Unknown_Instant_Var with To_String (Var);
      end if;

      if Item.Stop_Fixed and Item.Start_Fixed then
         Item.Elapsed_Time := Item.Stop_At - Item.Start_At;
      end if;
   end Fix_Instant;


   -----------
   -- After --
   -----------

   function After (List : Node_Label_Lists.Vector)
                   return Instant_Raw
   is
      use type Ada.Containers.Count_Type;

      ------------
      -- End_Of --
      ------------

      function End_Of (X : Node_Label) return String
      is (To_String (Join (Dotted_Identifier (X), Event_Names.End_Name)));
   begin
      if List.Length = 0 then
         raise Constraint_Error;

      elsif List.Length = 1 then
         return Instant_Raw (End_Of (List.First_Element));

      else
         declare
            Tmp : Unbounded_String := To_Unbounded_String ("max(");
         begin
            for K in List.First_Index .. List.Last_Index loop
               Tmp := Tmp & End_Of (List (K));

               if K < List.Last_Index then
                  Tmp := Tmp & ",";
               else
                  Tmp := Tmp & ")";
               end if;
            end loop;

            return Instant_Raw (To_String (Tmp));
         end;
      end if;
   end After;

   ----------------
   -- Add_Effort --
   ----------------

   procedure Add_Effort
     (Item    : in out Action_Node;
      Partner : in Nodes.Partners.Partner_Label;
      PM      : in Efforts.Person_Months)
   is
      use type Efforts.Person_Months;
   begin
      if not Item.Partner_Effort.Contains (Partner) then
         raise Unknown_Partner;
      end if;

      if Item.Partner_Effort (Partner) /= 0 then
         raise Duplicated_Effort;
      end if;

      Item.Partner_Effort (Partner) := PM;
   end Add_Effort;


   ------------------------
   -- Initialize_Efforts --
   ------------------------

   procedure Initialize_Efforts (Item     : in out Action_Node;
                                 Node_Dir : Node_Tables.Node_Table)
   is
   begin
      for Lab of Node_Dir.Labels_Of (Partner_Node) loop
         Item.Partner_Effort.Insert (Key      => Partners.Partner_Label (Lab),
                                     New_Item => 0);
      end loop;
   end Initialize_Efforts;

end EU_Projects.Nodes.Action_Nodes;
