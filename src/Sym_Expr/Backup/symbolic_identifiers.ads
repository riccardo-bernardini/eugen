
with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;

with Ada.Strings.Bounded;


package Symbolic_Identifiers is
   Max_ID_Length : constant Positive := 256;

   package Bounded_IDs is
     new Ada.Strings.Bounded.Generic_Bounded_Length (Max_ID_Length);

   subtype Bounded_ID is Bounded_IDs.Bounded_String;


   -- A valid identifier:
   --   * every character is a letter, a digit or an underscore
   --   * the first character must be a letter
   --   * the last character cannot be an underscore
   --   * it cannot have two consecutive underscores

   function Is_Valid_Simple_Name (X : String) return Boolean
   is (
       (X'Length > 0)

       and then (Is_Letter (X (X'First))
                 and Is_Alphanumeric (X (X'Last)))

       and then (for all I in X'Range =>
                       (Is_Alphanumeric (X (I))
                        or (X (I) = '_' and Is_Alphanumeric (X (I + 1)))))
      );

   --
   -- Function to check if X satisfies the syntax of IDs.  Inofrmally, an
   -- ID has a syntax similar to an Ada identifier with the following additions
   --
   --  (1)  "." can be used inside the identifier so that "foo.bar" is OK
   --  (2)  An identifier cannot begin nor end with "_" or "."
   --  (3)  "_" must always be followed by an alphanumeric char
   --  (4)  "." must always be followed by a letter
   --
   function Is_Valid_Variable (X : String) return Boolean
   is (
       (X'Length > 0)
       and then (X'Length <= Max_ID_Length)
       and then (Is_Letter (X (X'First))
                 and Is_Alphanumeric (X (X'Last)))
       and then (for all I in X'Range =>
                       (Is_Alphanumeric (X (I))
                        or else (X (I) = '_' and then Is_Alphanumeric (X (I + 1)))
                        or else (X (I) = '.' and then Is_Letter (X (I + 1)))))
       -- Note : in the condition above I+1 is always well defined since
       -- if X(X'Last) is alphanumeric, the last two tests are cutted
       -- away by the "or else" after  Is_Alphanumeric (X (I)), otherwise
       -- if X(X'Last) is not alphanumeric, the "for all" will not checked
       -- at all because of the preceding "and then"
      );


   type Simple_Identifier is new Bounded_IDs.Bounded_String
     with Dynamic_Predicate =>
       Is_Valid_Simple_Name (Bounded_IDs.To_String (Bounded_ID (Simple_Identifier)));

   subtype Function_Name is Simple_Identifier;

   function To_ID (X : String) return Simple_Identifier
   is (Simple_Identifier (Bounded_IDs.To_Bounded_String (X)))
     with Pre =>
       Is_Valid_Simple_Name (X);


   type Variable_Name is new Bounded_IDs.Bounded_String
     with Dynamic_Predicate =>
       Is_Valid_Variable (Bounded_IDs.To_String (Bounded_ID (Variable_Name)));

   function To_Var (X : String) return Variable_Name
   is (Variable_Name (Bounded_IDs.To_Bounded_String (X)))
     with Pre =>
       Is_Valid_Variable (X);

   function Join (Var   : Variable_Name;
                  Field : Simple_Identifier) return Variable_Name
   is (Var & "." & Variable_Name (Field));




end Symbolic_Identifiers;
