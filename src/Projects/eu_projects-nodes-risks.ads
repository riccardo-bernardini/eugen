
package EU_Projects.Nodes.Risks is
   type Risk_Descriptor is new Nodes.Node_Type with private;
   type Risk_Access is access Risk_Descriptor;

   type Risk_Label is new Dotted_Identifier;
   type Risk_Index is new Nodes.Node_Index;

   type Risk_Severity is (Very_Small, Small, Medium, Large, Very_Large);
   type Risk_Likeness is (Very_Small, Small, Medium, Large, Very_Large);

   function New_Risk (Label           : Risk_Label;
                      Description     : String;
                      Countermeasures : String;
                      Severity        : Risk_Severity;
                      Likeness        : Risk_Likeness)
                      return Risk_Access;

   function Label (Item : Risk_Descriptor) return Risk_Label;
   function Description (Item : Risk_Descriptor) return String;
   function Countermeasures (Item : Risk_Descriptor) return String;
   function Severity (Item : Risk_Descriptor) return Risk_Severity;
   function Likeness (Item : Risk_Descriptor) return Risk_Likeness;

   function Full_Index (Item     : Risk_Descriptor;
                        Prefixed : Boolean) return String;

   pragma Warnings (Off);
   function Get_Symbolic_Instant (Item : Risk_Descriptor;
                                  Var  : Simple_Identifier)
                                  return Times.Time_Expressions.Symbolic_Instant
   is (raise Unknown_Instant_Var);

   function Get_Symbolic_Duration (Item : Risk_Descriptor;
                                   Var  : Simple_Identifier)
                                   return Times.Time_Expressions.Symbolic_Duration
   is (raise Unknown_Duration_Var);

   function Dependency_List (Item : Risk_Descriptor)
                             return Node_Label_Lists.Vector
   is (Node_Label_Lists.Empty_Vector);

   pragma Warnings (On);

private
   type Risk_Descriptor is
     new Nodes.Node_Type
   with record
      Countemeasures : Unbounded_String;
      Severity       : Risk_Severity;
      Likeness       : Risk_Likeness;
   end record;


   function Full_Index (Item     : Risk_Descriptor;
                        Prefixed : Boolean) return String
   is ("R" & Item.Index_Image);

end EU_Projects.Nodes.Risks;
