with Ada.Characters;
with Ada.Characters.Handling;
package body Project_Processor.Projects.Identifiers is

   -------------------------
   -- Is_Valid_Identifier --
   -------------------------

   function Is_Valid_Identifier (X : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      if X'Length = 0 then
         return False;
      end if;

      if not Is_Letter (X (X'First)) then
         return False;
      end if;

      return (for all I in X'Range =>
        (Is_Letter (X(I))
         or Is_Decimal_Digit (X (I))
         or X (I) = '_'
         or X (I) = '-'));
   end Is_Valid_Identifier;

   -----------
   -- To_ID --
   -----------

   function To_ID (X : String) return Identifier is
   begin
      if not Is_Valid_Identifier (X) then
         raise Constraint_Error;
      end if;

      return Identifier (ID_Names.To_Bounded_String (X));
   end To_ID;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Identifier) return String is
   begin
      return ID_Names.To_String (ID_Names.Bounded_String (X));
   end To_String;

end Project_Processor.Projects.Identifiers;
