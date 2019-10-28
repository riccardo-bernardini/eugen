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

with Symbolic_Expressions;

package Test is
   function Get_Int (X : String) return String;

   type Int_Array is array (Positive range <>) of Integer;

   function Call (Name : String; Param : Int_Array)
                  return Integer;


   package Int_Expr is
     new Symbolic_Expressions (Scalar_Type  => Integer,
                               Scalar_Array => Int_Array,
                               Read_Scalar  => Get_Int,
                               Value        => Integer'Value,
                               Image        => Integer'Image);

end Test;
