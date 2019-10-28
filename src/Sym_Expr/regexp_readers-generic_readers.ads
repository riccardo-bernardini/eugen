----------------------------------------
-- Symbolic_Expressions.Regexp_Reader --
----------------------------------------

generic
   type Result_Type is private;

   Regexp : Regexp_Spec;

   with function Convert (X : String) return Result_Type;
package Regexp_Readers.Generic_Readers is
   procedure Reader (Input    : in     String;
                     Success  :    out Boolean;
                     Consumed :    out Natural;
                     Result   :    out Result_Type);
end Regexp_Readers.Generic_Readers;
