with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package Regexp_Readers is
   type Regexp_Spec is
      record
         Regexp               : Unbounded_String;
         Sub_Expression_Index : Natural;
      end record;

   function "+" (X : String) return Unbounded_String
             renames To_Unbounded_String;

   Ada_Identifier    : constant Regexp_Spec :=
                         (+"[[:alpha:]][[:alnum:]]*(_[[:alnum:]]+)*", 0);

   Dotted_Identifier : constant Regexp_Spec :=
                         (+"[[:alpha:]][[:alnum:]]*((_[[:alnum:]]+)|(\.[[:alpha:]][[:alnum:]]*))*", 0);

   Int_Regexp : constant Regexp_Spec :=
                  (+"[-+]?[[:space:]]*[[:digit:]]+(_[[:digit:]]+)*", 0);
end Regexp_Readers;
