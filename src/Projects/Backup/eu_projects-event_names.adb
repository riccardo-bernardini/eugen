with EU_Projects.Identifiers;

package body EU_Projects.Event_Names is
   use Identifiers;

   function Event_Label (E : Event_Class)
                               return String
   is
   begin
      case E is
         when Start_Time =>
            return "start";

         when End_Time =>
            return "end";

         when Duration_Time =>
            return "duration";
      end case;
   end Event_Label;

   -------------------
   -- Milestone_Var --
   -------------------

   function Milestone_Var
     (Label : Identifiers.Identifier)
      return String
   is
   begin
      return Image (Identifier (Label));
   end Milestone_Var;

   ---------------------
   -- Deliverable_Var --
   ---------------------

   function Deliverable_Var
     (Parent_WP : WPs.WP_Label;
                             Label     : Identifiers.Identifier)
      return String
   is
   begin
      return Image (Identifier (Parent_WP))
            & "." & Image (Identifier (Label));
   end Deliverable_Var;

   --------------
   -- Task_Var --
   --------------

   function Task_Var
     (Parent_WP : WPs.WP_Label;
                      Label     : Identifiers.Identifier;
                      Event     : Event_Class)
      return String
   is
   begin
      return Image (Identifier (Parent_WP))
            & "." & Image (Identifier (Label))
        & "." & Event_Label (Event);
   end Task_Var;

   function WP_Var
     (Label     : WPs.WP_Label;
      Event     : Event_Class)
      return String
   is
   begin
      return Image (Identifier (Label)) & "." & Event_Label (Event);
   end WP_Var;

end EU_Projects.Event_Names;
