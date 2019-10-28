with Tokenize.Private_Token_Lists;

package body Tokenize with SPARK_Mode => On is
   use Private_Token_Lists;

   function To_Array (List : Token_List) return Token_Array;

   function Basic_Split (To_Be_Splitted  : String;
                         Separator       : Ada.Strings.Maps.Character_Set;
                         Max_Token_Count : Token_Limit;
                         Collate         : Boolean)
                         return Token_List
         with
               Pre => To_Be_Splitted'Length > 0
               and To_Be_Splitted'Last < Integer'Last - 1,
               Annotate => (Gnatprove, Terminating);

   -----------------
   -- Basic_Split --
   -----------------

   function Basic_Split (To_Be_Splitted  : String;
                         Separator       : Ada.Strings.Maps.Character_Set;
                         Max_Token_Count : Token_Limit;
                         Collate         : Boolean)
                         return Token_List is
      use Ada.Strings.Maps;

      pragma Assert (To_Be_Splitted'Length > 0);


      Result : Token_List := Create (To_Be_Splitted'Length);

      First : Integer;
   begin
      pragma Assert (Result.Capacity = To_Be_Splitted'Length);
      pragma Assert (Result.Length = 0);

      First := To_Be_Splitted'First;
      for Pos in To_Be_Splitted'Range loop
         pragma Assert (Pos >= First);
         pragma Assert (Integer (Result.Length) <= Pos - To_Be_Splitted'First);

         if Is_In (To_Be_Splitted (Pos), Separator) then
            if First = Pos then
               if not Collate then
                  Result.Append ("");
               end if;
            else
               pragma Assert (First < Pos);

               Result.Append (To_Be_Splitted (First .. Pos - 1));
            end if;

            First := Pos + 1;

            if Max_Token_Count /= No_Limit and then
                  Result.Length = Max_Token_Count - 1
            then
               Result.Append (To_Be_Splitted (First .. To_Be_Splitted'Last));
               return Result;
            end if;

            pragma Assert (Integer (Result.Length) <= Pos - To_Be_Splitted'First + 1);

         end if;
      end loop;

      pragma Assert (if Max_Token_Count /= No_Limit then Result.Length < Max_Token_Count - 1);

      if First = To_Be_Splitted'Last + 1 then
         -- If I am here, to_be_splitted(to_be_splitted'last) is a separator
         -- That is, the string ends with a terminator.
         if not Collate then
            Result.Append ("");
         end if;
      else
         Result.Append (To_Be_Splitted (First .. To_Be_Splitted'Last));
      end if;


      return Result;
   end Basic_Split;
   --
   --     function Collated_Split (To_Be_Splitted  : String;
   --                              Separator       : Ada.Strings.Maps.Character_Set;
   --                              Max_Token_Count : Token_Limit)
   --                              return Token_List
   --           with Pre => Max_Token_Count /= No_Limit and To_Be_Splitted /= "",
   --           Post => Collated_Split'Result.Length > 0;
   --
   --     --------------------
   --     -- Collated_Split --
   --     --------------------
   --
   --     function Collated_Split (To_Be_Splitted  : String;
   --                              Separator       : Ada.Strings.Maps.Character_Set;
   --                              Max_Token_Count : Token_Limit)
   --                              return Token_List is
   --        Tokens : constant Token_List := Uncollated_Split (To_Be_Splitted, Separator);
   --        Result : Token_List := Create (Tokens.Length);
   --     begin
   --        if Max_Token_Count /= No_Limit then
   --           pragma Compile_Time_Warning (True, "Collated with limit unimplemented");
   --           raise Program_Error with "Collated with limit unimplemented";
   --        end if;
   --
   --        for K in 1 .. Tokens.Length loop
   --           if Tokens.Element (K) /= "" then
   --              Result.Append (Tokens.Element (K));
   --           end if;
   --        end loop;
   --
   --        return Result;
   --     end Collated_Split;



   function Split (To_Be_Splitted    : String;
                   Separator         : Ada.Strings.Maps.Character_Set;
                   Collate_Separator : Boolean;
                   Max_Token_Count   : Token_Limit := No_Limit)
                   return Token_Array is
   begin
      if To_Be_Splitted = "" then
         declare
            Empty : constant Token_Array (1 .. 0) := (others => <>);
         begin
            return Empty;
         end;
      end if;

      if Max_Token_Count = 1 then
         -- Easy case
         return (1 => To_Unbounded_String (To_Be_Splitted));
      end if;

      pragma Assert (To_Be_Splitted /= "");

      return To_Array (Basic_Split (To_Be_Splitted  => To_Be_Splitted,
                                    Separator       => Separator,
                                    Max_Token_Count => Max_Token_Count,
                                    Collate         => Collate_Separator));
      --        if (Collate_Separator) then
      --           return To_Array (Basic_Split (To_Be_Splitted, Separator, Max_Token_Count, True));
      --        else
      --           return To_Array (Basic_Split (To_Be_Splitted, Separator, Max_Token_Count, False));
      --        end if;
   end Split;

   --     -----------
   --     -- Split --
   --     -----------
   --
   --     function Split (To_Be_Splitted    : String;
   --                     Separator         : Character;
   --                     Collate_Separator : Boolean)
   --                     return Token_Array is
   --     begin
   --        return
   --     end Split;


   --------------
   -- To_Array --
   --------------

   function To_Array (List : Token_List)
                      return Token_Array is
      Result : Token_Array (1 .. List.Length);
   begin
      for I in Result'Range loop
         Result (I) := To_Unbounded_String (List.Element (I));
      end loop;

      return Result;
   end To_Array;


   procedure Head_And_Tail (To_Be_Splitted : String;
                            Separator      : Character;
                            Head           : out Unbounded_String;
                            Tail           : out Unbounded_String;
                            Trimming       : Trimming_Action := Both;
                            Default_Tail   : String := "")
   is

      function Trim (X : Unbounded_String) return Unbounded_String
      is (case Trimming is
             when None  => X,
             when Left  => Trim (X, Ada.Strings.Left),
             when Right => Trim (X, Ada.Strings.Right),
             when Both  => Trim (X, Ada.Strings.Both));

      Pieces : constant Token_Array := Split (To_Be_Splitted    => To_Be_Splitted,
                                              Separator         => Separator,
                                              Max_Token_Count   => 2);
   begin
      case Pieces'Length is
         when 0 =>
            raise Constraint_Error with "Empty input";

         when 1 =>
            Head := Trim (Pieces (Pieces'First));
            Tail := To_Unbounded_String (Default_Tail);

         when 2 =>
            Head := Trim (Pieces (Pieces'First));
            Tail := Trim (Pieces (Pieces'First + 1));

         when others =>
            -- If we arrive here, Split has a bug
            raise Program_Error;
      end case;
   end Head_And_Tail;
end Tokenize;
--        Current := To_Be_Splitted'First;
--
--    Main_Loop:
--        while Current <= To_Be_Splitted'Last loop
--
--       Search_For_Begin:
--           -- Since we are doing a Collated split, we need to skip
--           -- all the separators
--           while Current <= To_Be_Splitted'Last and then
--             To_Be_Splitted(Current) = Separator loop
--              Current := Current+1;
--           end loop Search_For_Begin;
--
--           -- If I am here or Current points after the end of
--           -- the string of To_Be_Splitted(Current) is a non-sep
--           -- character
--
--           exit when (Current > To_Be_Splitted'Last);
--
--           -- If I am here, To_Be_Splitted(Current) is a
--           -- non-separator character
--
--           First := Current;
--
--       Search_For_End:
--           while Current <= To_Be_Splitted'Last and then
--             To_Be_Splitted(Current) /= Separator loop
--              Current := Current+1;
--           end loop Search_For_End;
--
--           String_Vectors.Append (Result,
--                                  To_Be_Splitted(First..Current-1));
--
--           Current := Current+1;
--        end loop Main_Loop;

