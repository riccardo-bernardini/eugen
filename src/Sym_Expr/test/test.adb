----------------------------------------------------------------------------
--            Symbolic Expressions (symexpr)
--
--               Copyright (C) 2012, Riccardo Bernardini
--
--      This file is part of symexpr.
--
--      symexpr is free software: you can redistribute it and/or modify
--      it under the terms of the Lesser GNU General Public License as published by
--      the Free Software Foundation, either version 3 of the License, or
--      (at your option) any later version.
--
--      symexpr is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the Lesser GNU General Public License
--      along with gclp.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
package body Test is
   function Call(Name : String; Param : Int_Array)
                 return Integer
   is
      Result : Integer;
   begin
      if Name = "min" then
         if Param'Length = 0 then
            raise Constraint_Error;
         end if;

         Result := Param (Param'First);
         for I in Param'Range loop
            Result := Integer'Min (Result, Param (I));
         end loop;

      elsif Name = "max" then
         if Param'Length = 0 then
            raise Constraint_Error;
         end if;

         Result := Param (Param'First);
         for I in Param'Range loop
            Result := Integer'Max (Result, Param (I));
         end loop;
      elsif Name = "magic" then
         if Param'Length /= 0 then
            raise Constraint_Error;
         end if;

         Result := 42;
      else
         raise Constraint_Error;
      end if;

      return Result;
   end Call;

   function Get_Int (X : String) return String
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      Index : constant Natural := Index_Non_Blank (X);
      First : Positive;
      Last  : Natural;

      Int_Chars : constant Character_Set := To_Set (Character_Range'('0', '9'));
   begin
--        Ada.Text_Io.Put_Line ("Called [" & X & "]");

      if Index = 0 or else not  Is_In (X (Index), Int_Chars) then
         return "";
      end if;

      Find_Token (Source => X (Index .. X'Last),
                  Set    => Int_Chars,
                  Test   => Ada.Strings.Inside,
                  First  => First,
                  Last   => Last);

      --  Last should not be zero since we checked that X(Index) is a digit
      pragma Assert (Last /= 0);

      return X (First .. Last);
   end Get_Int;

end Test;
