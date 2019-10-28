with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with GNAT.Regpat;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Classified_Line (<>) is private ;
package Line_Arrays.Regexp_Classifiers is
   type Callback_Type is
         access function (Item    : String;
                          Matches : Gnat.Regpat.Match_Array)
                          return Classified_Line;

   type Regexp_Pair is private;

   function Is_Default (X : Regexp_Pair) return Boolean;

   function P (Regexp : String; Callback : Callback_Type) return Regexp_Pair
         with
               Pre => Regexp /= "",
               Post => not Is_Default (P'Result);

   function "+" (Callback : Callback_Type) return Regexp_Pair
         with Post => Is_Default ("+"'Result);


   type Regexp_Array is array (Positive range <>) of Regexp_Pair;

   type Classifier_Type (<>) is tagged private;

   function Create (Regexps : Regexp_Array) return Classifier_Type
         with Pre =>
               (for all I in Regexps'Range =>
                      (for all J in I + 1 .. Regexps'Last =>
                             (not (Is_Default (Regexps (I)) and Is_Default (Regexps (J))))
                      )
               );
   --  The precondition expressed in plain English requires that at most
   --  one entry of Regexps must be a default expression


   function Classify (Classifier : Classifier_Type;
                      Line       : String) return Classified_Line;

   Double_Default : exception;

private
   type Regexp_Pair is
      record
         Regexp   : Unbounded_String;
         Callback : Callback_Type;
      end record;

   function Is_Default (X : Regexp_Pair) return Boolean
   is (X.Regexp = Null_Unbounded_String);

   function P (Regexp : String; Callback : Callback_Type) return Regexp_Pair
   is (Regexp_Pair'(To_Unbounded_String(Regexp), Callback));

   function "+" (Callback : Callback_Type) return Regexp_Pair
   is (Regexp_Pair'(Null_Unbounded_String, Callback));

   type Regexp_Descriptor (Size : Gnat.Regpat.Program_Size) is
      record
         Matcher  : GNAT.Regpat.Pattern_Matcher (Size);
         Callback : Callback_Type;
      end record;

   package Descriptor_Lists is
         new Ada.Containers.Indefinite_Doubly_Linked_Lists (Regexp_Descriptor);

   procedure Add (To       : in out Classifier_Type;
                  Regexp   : String;
                  Callback : Callback_Type);

   procedure Add_Default (To       : in out Classifier_Type;
                          Callback : Callback_Type);


   type Classifier_Type is tagged
      record
         Exprs   : Descriptor_Lists.List := Descriptor_Lists.Empty_List;
         Default : Callback_Type := null;
      end record;
end Line_Arrays.Regexp_Classifiers;
