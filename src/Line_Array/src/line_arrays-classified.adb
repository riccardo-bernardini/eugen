pragma Ada_2012;
package body Line_Arrays.Classified is

   --------------
   -- Classify --
   --------------


   function Classify (Classifier : Classifier_Type;
                      Lines      : Line_Array)
                      return Classified_Line_Vectors.Vector
   is
      Result : Classified_Line_Vectors.Vector;
   begin
      Result.Clear;

      for Line of Lines loop
         declare
            C_Line  : constant Classified_Line := Classify (Classifier, Line);
         begin
            if not Is_To_Be_Ignored (C_Line) then
               Result.Append (C_Line);
            end if;
         end;
      end loop;

      return Result;
   end Classify;

end Line_Arrays.Classified;
