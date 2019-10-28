pragma Ada_2012;
package body Tokenize.Token_Vectors is

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (X : Token_Array) return Vector is
      Result : Vector;
   begin
      for Tk of X loop
         Result.Append (To_String (Tk));
      end loop;

      return Result;
   end To_Vector;
end Tokenize.Token_Vectors;
