with Ada.Strings.Fixed;

package Project_Processor is
   -- The result of function 'Image associated to discrete types has
   -- a space at the beginning.  That space is quite annoying and needs
   -- to be trimmed.  This function is here so that everyone can use it

   function Chop (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Image (X : Integer) return String
   is (Chop (Integer'Image (X)));
end Project_Processor;
