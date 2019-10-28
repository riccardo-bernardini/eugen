with Ada.Containers.Indefinite_Vectors;

generic
   type Classified_Line (<>) is private;
   type Classifier_Type (<>) is private;

   with function Classify (Classifier : Classifier_Type;
                           Line       : String) return Classified_Line is <> ;

   with function Is_To_Be_Ignored (Line : Classified_Line) return  Boolean is <>;
package Line_Arrays.Classified is
   package Classified_Line_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Classified_Line);

   function Classify (Classifier : Classifier_Type;
                      Lines      : Line_Array)
                      return Classified_Line_Vectors.Vector;
end Line_Arrays.Classified;
