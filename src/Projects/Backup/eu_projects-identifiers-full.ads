with Ada.Containers.Vectors;

package EU_Projects.Identifiers.Full is
   use type Ada.Containers.Count_Type;

   type Segment_Index is range 1 .. Integer'Last;

   type Full_ID is private;

   Empty_Path : constant Full_ID;

   function "=" (X, Y : Full_ID) return Boolean;
   function "<" (X, Y : Full_ID) return Boolean;


   -- A valid path has a syntax similar to an identifier, with the
   -- following exceptions
   --
   --  * It can have also dots '.'
   --  * It cannot end with a dot
   --  * After a dot a letter must follow
   function Is_Valid_Path (X : String) return Boolean
   is (
       (X'Length > 0)
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


   function Parse (X : String) return Full_ID
         with Pre => Is_Valid_Path (X);

   function Image (X : Full_ID) return String;

   function Length (Item : Full_ID) return Ada.Containers.Count_Type;

   function Segment (Item : Full_ID; Index : Segment_Index) return Identifier;

   function First (Item : Full_ID) return Identifier with
         Pre => Length (Item) /= 0;

   function Last (Item : Full_ID) return Identifier with
         Pre => Length (Item) /= 0;

   function Header (Item : Full_ID) return Full_ID with
         Pre => Length (Item) /= 0,
         Post => Length (Header'Result) = Length (Item)-1;

   function Trailer (Item : Full_ID) return Full_ID  with
         Pre => Length (Item) /= 0,
         Post => Length (Trailer'Result) = Length (Item)-1;

   type Resolver_Type is limited interface;

   function Find (Resolver : Resolver_Type;
                  What     : Full_ID)
                  return Boolean
                  is abstract;

   function Resolve (Item         : Full_ID;
                     Prefix       : Full_ID;
                     Resolver     : Resolver_Type'Class)
                     return Full_ID;

private
   package ID_Vectors is
         new Ada.Containers.Vectors (Index_Type   => Segment_Index,
                                     Element_Type => Identifier);


   type Full_ID is
      record
         Segments : ID_Vectors.Vector;
      end record;


   Empty_Path : constant Full_ID := Full_ID'(Segments => ID_Vectors.Empty_Vector);


   function Length (Item : Full_ID) return Ada.Containers.Count_Type
   is (Item.Segments.Length);

   function Segment (Item : Full_ID; Index : Segment_Index) return Identifier
   is (Item.Segments.Element (Index));

   function First (Item : Full_ID) return Identifier
   is (Item.Segments.First_Element);

   function Last (Item : Full_ID) return Identifier
   is (Item.Segments.Last_Element);


   function "=" (X, Y : Full_ID) return Boolean
   is (ID_Vectors."="(X.Segments, Y.Segments));


end EU_Projects.Identifiers.Full;
