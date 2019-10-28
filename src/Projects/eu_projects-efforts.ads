with Ada.Containers.Ordered_Maps;

with EU_Projects.Nodes.Partners;

package EU_Projects.Efforts is
   use type Nodes.Partners.Partner_Label;

   type Person_Months is new Natural;

   package Effort_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Nodes.Partners.Partner_Label,
                                      Element_Type => Person_Months);

   function Parse (Spec : String) return Effort_Maps.Map;
   Bad_Effort_List : exception;
end EU_Projects.Efforts;
