with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Strings.Bounded;

package EU_Projects.Identifiers is
   type Identifier is  private;

   function "=" (X, Y : Identifier) return Boolean;
   function "<" (X, Y : Identifier) return Boolean;
   function Join (X, Y : Identifier) return Identifier;

   -- A valid identifier:
   --   * every character is a letter, a digit or an underscore
   --   * the first character must be a letter
   --   * the last character cannot be an underscore
   --   * it cannot have two consecutive underscores
   function Is_Valid_Identifier (X : String) return Boolean
   is (
       (X'Length > 0)

       and then (Is_Letter (X (X'First))
                 and Is_Alphanumeric (X (X'Last)))

       and then (for all I in X'Range =>
                       (Is_Alphanumeric (X (I))
                        or (X (I) = '_' and Is_Alphanumeric (X (I + 1)))))
      );


   function To_ID (X : String) return Identifier
         with Pre => Is_Valid_Identifier (X);

   function Image (X : Identifier) return String;

   Bad_Identifier : exception;
private
   package ID_Names is new
         Ada.Strings.Bounded.Generic_Bounded_Length (16);

   use type ID_Names.Bounded_String;

   type Identifier is
      record
         ID : ID_Names.Bounded_String;
      end record;



   function Image (X : Identifier) return String
   is (ID_Names.To_String (ID_Names.Bounded_String (X.ID)));

   function "=" (X, Y : Identifier) return Boolean
   is (ID_Names.Bounded_String (X.ID) = ID_Names.Bounded_String (Y.ID));


   function "<" (X, Y : Identifier) return Boolean
   is (ID_Names.Bounded_String (X.ID) > ID_Names.Bounded_String (Y.ID));

   function Join (X, Y : Identifier) return Identifier
   is (ID => X.ID & "." & Y.ID);


end EU_Projects.Identifiers;
