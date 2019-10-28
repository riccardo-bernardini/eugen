with Ada.Finalization; use Ada.Finalization;
with EU_Projects.Nodes.Action_Nodes.WPs;

package body EU_Projects.Nodes.Timed_Nodes.Milestones is

   ------------
   -- Create --
   ------------

   function Create
     (Label        : Milestone_Label;
                    Name         : String;
                    Short_Name   : String;
                    Description  : String;
                    Due_On       : String;
                    Node_Dir     : in out Node_Tables.Node_Table;
                    Verification : String)
      return Milestone_Access
   is
      Result : Milestone_Access;
   begin
      Result := new Milestone'(Controlled with
                                 Label             => Node_Label (Label),
                               Name              => To_Unbounded_String (Name),
                               Short_Name        => Shortify (Short_Name, Name),
                               Class             => Milestone_Node,
                               Index             => No_Milestone,
                               Description       => To_Unbounded_String (Description),
                               Attributes        => Attribute_Maps.Empty_Map,
                               Expected_On       => <>,
                               Expected_Raw      => To_Unbounded_String (Due_On),
                               Expected_Symbolic => <>,
                               Deliv             => Node_Label_Lists.Empty_Vector,
                               Fixed             => False,
                               Verification      => To_Unbounded_String (Verification));

      Node_Dir.Insert (ID   => Node_Label (Label),
                       Item => Node_Access (Result));

      return Result;
   end Create;

   ---------------------
   -- Add_Deliverable --
   ---------------------

   procedure Add_Deliverable (Item  : in out Milestone;
                              Deliv : Node_Label)
   is
   begin
      Item.Deliv.Append (Deliv);
   end Add_Deliverable;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (Item : in out Milestone;
                        Idx  : Milestone_Index)
   is
   begin
      if Item.Index /= No_Index and Item.Index /= Node_Index (Idx) then
         raise Constraint_Error;
      else
         Item.Index := Node_Index (Idx);
      end if;
   end Set_Index;

   ------------------
   -- Update_Index --
   ------------------

   procedure Update_Index (Item : in out Milestone;
                           Idx  : Milestone_Index)
   is
   begin
      if Item.Index = No_Index then
         raise Constraint_Error;
      else
         Item.Index := Node_Index (Idx);
      end if;
   end Update_Index;


end EU_Projects.Nodes.Timed_Nodes.Milestones;
