--  with EU_Projects.Nodes.Action_Nodes.WPs;
--  with EU_Projects.Nodes.Action_Nodes.Tasks;
--  with EU_Projects.Nodes.Timed_Nodes.Deliverables;
--  with EU_Projects.Nodes.Timed_Nodes.Milestones;
--  with Ada.Tags;
--  with EU_Projects.Nodes.Partners;

package body EU_Projects.Nodes is


   -----------
   -- After --
   -----------

   function After (Labels   : Node_Label_Lists.Vector;
                   Done_Var : String) return String
   is
      Tmp   : Unbounded_String := Null_Unbounded_String; --To_Unbounded_String ("max(");
   begin
      if Labels.Is_Empty then
         if Tmp = Null_Unbounded_String then
            raise Bad_Input with "End time '*' with no parent";
         end if;
      end if;

      for El of Labels  loop
         if Tmp /= Null_Unbounded_String then
            Tmp := Tmp & ",";
         end if;

         Tmp := Tmp & To_String (El) & "." & Done_Var;
      end loop;


      return To_String ("max(" & Tmp & ")");
   end After;

   function Join (List      : Node_Label_Lists.Vector;
                  Separator : String)
                  return String
   is
      Result : Unbounded_String;
   begin
      for idx in List.Iterate loop
         Result := Result & Image (List (Idx));

         if Node_Label_Lists.To_Index (Idx) < List.Last_Index then
            Result := Result & Separator;
         end if;
      end loop;

      return To_String (Result);
   end Join;

   --------------
   -- Is_Fixed --
   --------------

   function Is_Fixed (Item : Node_Type;
                      Var  : Simple_Identifier)
                      return Boolean
   is
   begin
      raise Unknown_Instant_Var;
      return False;
   end Is_Fixed;

   -----------------
   -- Fix_Instant --
   -----------------

   procedure Fix_Instant
     (Item  : in out Node_Type;
      Var   : Simple_Identifier;
      Value : Times.Instant)
   is
   begin
      raise Unknown_Instant_Var with "Bad call to Fix_Instant";
   end Fix_Instant;

--     ------------------
--     -- Fix_Duration --
--     ------------------
--
--     procedure Fix_Duration
--       (Item  : in out Node_Type;
--        Var   : Simple_Identifier;
--        Value : Times.Duration)
--     is
--     begin
--        raise Unknown_Duration_Var with "Bad call to Fix_Duration";
--     end Fix_Duration;
   --     function Create (Label       : Identifiers.Identifier;
   --                      Name        : String;
   --                      Short_Name  : String;
   --                      Description : String;
   --                      Index       : Node_Index := No_Index)
   --                      return Node_Type
   --     is
   --        use Ada.Finalization;
   --     begin
   --        return Node_Type'(Controlled with
   --                            Label       => Label,
   --                          Name        => To_Unbounded_String (Name),
   --                          Short_Name  => To_Unbounded_String (Short_Name),
   --                          Index       => Index,
   --                          Description => To_Unbounded_String (Description),
   --                          Attributes  => Attribute_Maps.Empty_Map);
   --     end Create;


   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute (Item  : in out Node_Type;
                            Name  : in    String;
                            Value : in    String)
   is
   begin
      Item.Attributes.Include (Key      => Name,
                               New_Item => Value);
   end Add_Attribute;

   ----------------------
   -- Parse_Label_List --
   ----------------------

   function Parse_Label_List (Input : String) return Node_Label_Lists.Vector
   is
      Result : Node_Label_Lists.Vector;
   begin
      for ID of To_ID_List (Input) loop
         Result.Append (Node_Label (ID));
      end loop;

      return Result;
   end Parse_Label_List;


end EU_Projects.Nodes;


--
--     ---------------
--     -- Set_Index --
--     ---------------
--
--     procedure Set_Index (Item : in out Node_Type;
--                          Idx  : Node_Index)
--     is
--     begin
--        if Item.Index /= No_Index then
--           raise Constraint_Error;
--        end if;
--
--        Item.Index := Idx;
--     end Set_Index;
--


--     ----------
--     -- Is_A --
--     ----------
--
--     function Is_A (Item  : Node_Type'Class;
--                    Class : Node_Class)
--                    return Boolean
--     is
--        use type Ada.Tags.Tag;
--        Tags : constant array (Node_Class) of Ada.Tags.Tag :=
--                 (
--                  A_WP          => Action_Nodes.WPs.Project_WP'Tag,
--                  A_Task        => Action_Nodes.Tasks.Project_Task'Tag,
--                  A_Deliverable => Timed_Nodes.Deliverables.Deliverable'Tag,
--                  A_Milestone   => Timed_Nodes.Milestones.Milestone'Tag,
--                  A_Partner     => Nodes.Partners.Partner'Tag
--                 );
--     begin
--        return Item'Tag = Tags (Class);
--     end Is_A;

