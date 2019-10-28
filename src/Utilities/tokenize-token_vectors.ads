with Ada.Containers.Indefinite_Vectors;


package Tokenize.Token_Vectors is
   package String_Vectors is
         new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                                Element_Type => String);

   subtype Vector is String_Vectors.Vector;

   function To_Vector (X : Token_Array) return Vector;
end Tokenize.Token_Vectors;
