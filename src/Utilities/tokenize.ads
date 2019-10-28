--                              -*- Mode: Ada -*-
--  Filename        : tokenize.ads
--  Description     : Ruby-like split
--  Author          : Finta Tartaruga
--  Created On      : Tue Sep 11 22:05:53 2007
--  Last Modified By: R. Bernardini
--  Last Modified On: November 14, 2007
--  Update Count    : 1
--  Status          : <TESTED>

--
-- This package provides a function Split which divides its input
-- string in smaller strings, separated by a "separator" (much as the
-- split function in Perl, Ruby, and so on...).  Function Split returns
-- a Token_List (defined by this package)  whose elements can be accessed
-- by the function Element.
--
-- Function Split can accept a third Boolean value Collate_Separator.
-- If Collate_Separator is true, consecutive istances of the separator are
-- considered as a single one.  If Collate_Separator is False, for every
-- pair of consecutive separator characters an empty string will be returned.
-- Moreover, if Collate_Separator is True, any separator at the beginning of
-- the string is ignored.  Separators at the end are always ignored.
--
-- The default value of Collate_Separator is true if the separator
-- is the space, false otherwise.
--
-- Examples:
--
--   Split("Hello   there")               returns "Hello" and "there"
--   Split("Hello   there", ' ', False)   returns "Hello", "" and "there"
--   Split("Hello::there", ':')           returns "Hello", "" and "there"
--   Split("Hello::there", ':', True)     returns "Hello" and "there"
--
--  with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Strings.Maps;


package Tokenize with SPARK_Mode => On is
   type Token_Count is range 0 .. Integer'Last;
   subtype Token_Limit is Token_Count;

   No_Limit : constant Token_Limit := 0;

   type Token_Array is array (Token_Count range <>) of Unbounded_String;

   --
   -- Split string To_Be_Splitted in substring separated by any character in
   -- Separator.  If Collate_Separator is true consider consecutive
   -- istances of Separator as a single one.
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : Ada.Strings.Maps.Character_Set;
                   Collate_Separator : Boolean;
                   Max_Token_Count   : Token_Limit := No_Limit) return Token_Array
         with Pre => To_Be_Splitted'Length < Integer'Last - 1,
         Post =>
               ((To_Be_Splitted = "") = (Split'Result'Length = 0))
               and (if Max_Token_Count /= No_Limit then Split'Result'Length <= Max_Token_Count),
         Annotate => (Gnatprove, Terminating);
   --
   -- Synctactic sugar when only a single separator (and not a set) is
   -- used.
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : Character;
                   Collate_Separator : Boolean;
                   Max_Token_Count   : Token_Limit := No_Limit) return Token_Array
   is (Split (To_Be_Splitted    => To_Be_Splitted,
              Separator         => Ada.Strings.Maps.To_Set (Separator),
              Collate_Separator => Collate_Separator,
              Max_Token_Count   => Max_Token_Count))
         with Post =>
               ((To_Be_Splitted = "") = (Split'Result'Length = 0))
               and (if Max_Token_Count /= No_Limit then Split'Result'Length <= Max_Token_Count);

   --
   -- Synctactic sugar when only a single separator (and not a set) is
   -- used.
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : String;
                   Collate_Separator : Boolean;
                   Max_Token_Count   : Token_Limit := No_Limit) return Token_Array
   is (Split (To_Be_Splitted    => To_Be_Splitted,
              Separator         => Ada.Strings.Maps.To_Set (Separator),
              Collate_Separator => Collate_Separator,
              Max_Token_Count   => Max_Token_Count))
         with Post =>
               ((To_Be_Splitted = "") = (Split'Result'Length = 0))
               and (if Max_Token_Count /= No_Limit then Split'Result'Length <= Max_Token_Count);



   --
   -- Synctatic sugar with Separator defaulting to the space, with
   -- Collate_Separator True if Separator is the space, false otherwise
   --
   function Split (To_Be_Splitted    : String;
                   Separator         : Character := ' ';
                   Max_Token_Count   : Token_Limit := No_Limit)
                   return Token_Array
   is (Split (To_Be_Splitted, Separator, Separator = ' ', Max_Token_Count))
         with  Post =>
               ((To_Be_Splitted = "") = (Split'Result'Length = 0))
               and (if Max_Token_Count /= No_Limit then Split'Result'Length <= Max_Token_Count);

   type Trimming_Action is (None, Left, Right, Both);

   procedure Head_And_Tail (To_Be_Splitted : String;
                            Separator      : Character;
                            Head           : out Unbounded_String;
                            Tail           : out Unbounded_String;
                            Trimming       : Trimming_Action := Both;
                            Default_Tail   : String := "")
         with Pre => To_Be_Splitted'Length > 0;

end Tokenize;
