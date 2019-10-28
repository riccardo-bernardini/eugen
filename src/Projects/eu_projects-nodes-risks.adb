with Ada.Finalization; use Ada.Finalization;
package body EU_Projects.Nodes.Risks is

   --------------
   -- New_Risk --
   --------------

   function New_Risk
     (Label           : Risk_Label;
      Description     : String;
      Countermeasures : String;
      Severity        : Risk_Severity;
      Likeness        : Risk_Likeness)
      return Risk_Access
   is
   begin
      return new Risk_Descriptor'(Controlled with
                                    Label          => Node_Label (Label),
                                  Class          => Risk_Node,
                                  Name           => Null_Unbounded_String,
                                  Short_Name     => Null_Unbounded_String,
                                  Index          => No_Index,
                                  Description    => To_Unbounded_String (Description),
                                  Countemeasures => To_Unbounded_String (Countermeasures),
                                  Severity       => Severity,
                                  Likeness       => Likeness,
                                  Attributes     => <>);
   end New_Risk;

   -----------
   -- Label --
   -----------

   function Label (Item : Risk_Descriptor) return Risk_Label is
   begin
      return Risk_Label (Item.Label);
   end Label;

   -----------------
   -- Description --
   -----------------

   function Description (Item : Risk_Descriptor) return String is
   begin
      return To_String (Item.Description);
   end Description;

   ---------------------
   -- Countermeasures --
   ---------------------

   function Countermeasures (Item : Risk_Descriptor) return String is
   begin
      return To_String (Item.Countemeasures);
   end Countermeasures;

   --------------
   -- Severity --
   --------------

   function Severity (Item : Risk_Descriptor) return Risk_Severity is
   begin
      return Item.Severity;
   end Severity;

   --------------
   -- Likeness --
   --------------

   function Likeness (Item : Risk_Descriptor) return Risk_Likeness is
   begin
      return Item.Likeness;
   end Likeness;
end EU_Projects.Nodes.Risks;
