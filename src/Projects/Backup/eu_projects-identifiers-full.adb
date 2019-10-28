with Ada.Strings.Unbounded;
with Tokenize;

package body EU_Projects.Identifiers.Full is

   ---------
   -- ">" --
   ---------

   function "<" (X, Y : Full_ID) return Boolean is
      use ID_Vectors;

      Tx : ID_Vectors.Vector := X.Segments;
      Ty : ID_Vectors.Vector := Y.Segments;
   begin
      while not (Tx.Is_Empty or Ty.Is_Empty) loop
         if Tx.First_Element > Ty.First_Element then
            return False;
         elsif Tx.First_Element < Ty.First_Element then
            return True;
         else
            Tx.Delete_First;
            Ty.Delete_First;
         end if;
      end loop;

      --
      -- If Ty is not empty it means that Tx x, therefore X < Y.
      -- If Ty is empty, it can be
      --
      --     + Also Tx is empty => X = Y
      --     + Tx is not empty => X > Y
      --
      -- Therefore, X < Y iff Ty is empty
      --
      return not Ty.Is_Empty;
   end "<";

   -----------
   -- Parse --
   -----------

   function Parse
     (X : String)
      return Full_ID
   is
      use Ada.Strings.Unbounded;

      Segments : constant Tokenize.Token_List :=
                   Tokenize.Split (To_Be_Splitted    => X,
                                   Separator         => '.');

      Result : Full_ID;
   begin
      for Seg of Segments loop
         Result.Segments.Append (To_ID (To_String (Seg)));
      end loop;

      return Result;
   end Parse;

   -----------
   -- Image --
   -----------

   function Image (X : Full_ID) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for Segment of X.Segments loop
         if Result /= Null_Unbounded_String then
            Result := Result & ".";
         end if;

         Result := Result & Image(Segment);
      end loop;

      return To_String (Result);
   end Image;


   ------------
   -- Header --
   ------------

   function Header
     (Item : Full_ID)
      return Full_ID
   is
      Result : Full_ID := Item;
   begin
      Result.Segments.Delete_Last;
      return Result;
   end Header;

   -------------
   -- Trailer --
   -------------

   function Trailer
     (Item : Full_ID)
      return Full_ID
   is
      Result : Full_ID := Item;
   begin
      Result.Segments.Delete_First;
      return Result;
   end Trailer;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Item         : Full_ID;
      Prefix       : Full_ID;
      Resolver     : Resolver_Type'Class)
      return Full_ID
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Resolve unimplemented");
      raise Program_Error with "Unimplemented function Resolve";
      return Resolve (Item => Item, Prefix => Prefix, Resolver => Resolver);
   end Resolve;

end EU_Projects.Identifiers.Full;
