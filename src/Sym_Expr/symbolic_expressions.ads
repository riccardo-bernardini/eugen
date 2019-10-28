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

--
-- <summary>
--  I wrote the first version of this package a day when I wanted to allow
--  the user of a program to specify values in a symbolic form like
--  "max(intro.end, bluesheet.end)"  After writing the code that handled this
--  type of expression, I noticed that the code was general enough and with
--  minimal effort I converted it to a generic package that I succesively
--  improved.
--
--  This package allows you to handle expression in "symbolic" form inside
--  your program.  Expressions operate on "scalars" whose type parametrizes
--  this package (so you can have expressions of floats, integers or...
--  dates [my case]).
--
--  You can
--
--   (A)  Construct symbolic expression
--         =>  by parsing strings with Parse, e.g.,
--
--                 X := parse("2*3 - max(y, abs(u))")
--
--         =>  by constructing them using the provided constructors and
--             operators.  For example,
--
--                 X    := Variable("x");
--                 Poly := to_expr(12)*X*X - to_expr(4)*x + to_expr(1);
--                 Top  := Function_Call("max", Function_Call("abs", x), Poly);
--
--              in this case Top is an expression that represents the function
--
--                   max(abs(x), 12*x*x - 4*x + 1)
--
--    (B)  Replace variables in expression with scalars or other
--         expressions (function Replace)
--
--    (C)  Evaluate an expression.  For example, to plot a graph of Top(X)
--         you can use
--
--            for I in 1..10 loop
--              Plot(I, Eval(Replace(Top, "x", I)));
--            end loop;
--
--  As said above, this package is parameterized by the type of the scalar.
--  It requires that the usual operators (+, -, *, ...) are defined.
-- </summary>

--  with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

--  with Symbolic_Identifiers;

--
--
--

generic
   type Scalar_Type is  private;
   --  The type representing the scalars

   type Scalar_Array is array (Positive range <>) of Scalar_Type;

   type Identifier is private;

   with function "<" (L, R : Identifier) return Boolean is <>;

   with function "+" (X : Scalar_Type) return Scalar_Type is <>;
   with function "-" (X : Scalar_Type) return Scalar_Type is <>;

   with function "+" (L, R : Scalar_Type) return Scalar_Type is <>;
   with function "-" (L, R : Scalar_Type) return Scalar_Type is <>;
   with function "*" (L, R : Scalar_Type) return Scalar_Type is <>;
   with function "/" (L, R : Scalar_Type) return Scalar_Type is <>;

   with function Call (Name : Identifier; Param : Scalar_Array)
                       return Scalar_Type is <>;
   --  Callback used to evaluate the functions in a Symoblic_Expression
   --  Name is the name of the function, while Param will store the
   --  parameters given to the function


   with function Image (Item : Scalar_Type) return String;
   --  Return a text representation of a scalar

   with function ID_Image (Item : Identifier) return String;
   --  Return a text representation of a scalar
package Symbolic_Expressions is
--     use type Symbolic_Identifiers.Identifier;

   subtype Variable_Name is  Identifier;
   subtype Function_Name is  Identifier;

   type Symbolic_Expression is new
     Ada.Finalization.Controlled
   with
     private;

   type Expression_Array is array (Positive range <>) of Symbolic_Expression;


   function Is_Constant (X : Symbolic_Expression) return Boolean;
   --  Return true if X has no free variables

   Not_A_Scalar : exception;
   function Eval (X : Symbolic_Expression) return Scalar_Type;
   --  Evaluate expression X and return the corresponding scalar value.
   --  Raise Not_A_Scalar is Is_Constant(X) is false


   function To_Expr (X : Scalar_Type) return Symbolic_Expression;
   --  Return an expression representing the value of X

   function Variable (Name : Variable_Name) return Symbolic_Expression;
   -- Return an expression representing the given variable

   function Function_Call (Name          : Function_Name;
                           Parameters    : Expression_Array)
                           return Symbolic_Expression;
   -- Return an expression representing a function call

   function "+" (L : Symbolic_Expression) return Symbolic_Expression;
   function "-" (L : Symbolic_Expression) return Symbolic_Expression;

   function "+" (L, R : Symbolic_Expression) return Symbolic_Expression;
   function "-" (L, R : Symbolic_Expression) return Symbolic_Expression;
   function "*" (L, R : Symbolic_Expression) return Symbolic_Expression;
   function "/" (L, R : Symbolic_Expression) return Symbolic_Expression;

   function "+" (L : Symbolic_Expression; R : Scalar_Type) return Symbolic_Expression;
   function "-" (L : Symbolic_Expression; R : Scalar_Type) return Symbolic_Expression;
   function "*" (L : Symbolic_Expression; R : Scalar_Type) return Symbolic_Expression;
   function "/" (L : Symbolic_Expression; R : Scalar_Type) return Symbolic_Expression;
   function "+" (L : Scalar_Type; R : Symbolic_Expression) return Symbolic_Expression;
   function "-" (L : Scalar_Type; R : Symbolic_Expression) return Symbolic_Expression;
   function "*" (L : Scalar_Type; R : Symbolic_Expression) return Symbolic_Expression;
   function "/" (L : Scalar_Type; R : Symbolic_Expression) return Symbolic_Expression;



   package Variable_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Variable_Name);

   function Free_Variables (Item : Symbolic_Expression)
                            return Variable_Sets.Set;
   --  Return the name of the variables used in Item

   procedure Iterate_On_Vars (Item : Symbolic_Expression;
                              Process : access procedure (Var_Name : Variable_Name));
   -- Call Process for every variable in Item


   function Replace (Item     : Symbolic_Expression;
                     Var_Name : Variable_Name;
                     Value    : Scalar_Type)
                     return Symbolic_Expression;
   --  Replace every occurence of Var_Name in Item with the given scalar value


   package Variable_Tables is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Variable_Name,
        Element_Type => Scalar_Type);
   --  Maps Variable_Tables.Maps are used to associate values to
   --  variable names

   function Replace (Item  : Symbolic_Expression;
                     Table : Variable_Tables.Map)
                     return Symbolic_Expression;
   --  For every variable in Item, check if the variable name is a key
   --  of Table and, if found, replace every occurence of the variable
   --  with the corresponding value stored in Table.

   function Replace (Item     : Symbolic_Expression;
                     Var_Name : Variable_Name;
                     Value    : Symbolic_Expression)
                     return Symbolic_Expression;
   --  Replace every instance of variable Var_Name with the expression
   --  Value

   -- ============= --
   -- == PARSING == --
   -- ============= --



   -- =========== --
   -- == DEBUG == --
   -- =========== --

   function Dump (Item : Symbolic_Expression) return String;
   --  Return a textual representation of Item.  Useful for debugging.
   --  Currently it returns a tree-like string similar to
   --
   --     *
   --        Const  4
   --        -
   --           +
   --              Const  5
   --              Const  1
   --           Call max
   --              Var pluto.end
   --              Var pippo.end
   --
   --  The tree above is relative to the expression
   --
   --              4 * (5 + 1 - max (pluto.end, pippo.end))
   --
   --  Why does not it return a "human" string like the latter one?  Because
   --  Dump is for debugging, so it is more useful to be able to see the
   --  internal structure of Item.

private
--     --     use Ada.Strings.Unbounded;
--     package Bounded_IDs is
--       new Ada.Strings.Bounded.Generic_Bounded_Length (Symbolic_Identifiers.Max_ID_Length);

--     subtype Bounded_ID is Bounded_IDs.Bounded_String
--       with Dynamic_Predicate => Symbolic_Identifiers.Is_Valid_ID (Bounded_IDs.To_String (Bounded_ID));

--     function To_Bounded_ID (X : Symbolic_Identifiers.Variable_Name) return Bounded_ID
--     is (Bounded_IDs.To_Bounded_String (String (X)));
--
--     function To_String (X : Bounded_ID) return String
--     is (Bounded_IDs.To_String (X));

   use Ada.Finalization;

   --  A symbolic expression is stored as a tree, where each node has a
   --  "class" representing the operation associated with the node.

   type Node_Class is (Unary_Plus, Unary_Minus,
                       Sum, Sub, Mult, Div,
                       Fun_Call, Var, Const);

   type Node_Type (Class : Node_Class);

   type Node_Access is access Node_Type;

   type Parameter_Array is array (1..128) of Node_Access;

   type Node_Type (Class : Node_Class) is
      record
         case Class is
            when Unary_Plus | Unary_Minus =>
               Term : Node_Access;
            when Sum | Sub | Mult | Div =>
               Left, Right : Node_Access;
            when Fun_Call =>
               Fun_Name    : Function_Name;
               Parameters  : Parameter_Array;
               N_Params    : Natural;
            when Var =>
               Var_Name    : Variable_Name;
            when Const =>
               Value       : Scalar_Type;
         end case;
      end record;

   function Duplicate (Item : Node_Access) return Node_Access;
   pragma Precondition (Item /= null);
   --  Create a duplicate of the tree rooted in Item

   procedure Free (Item : in out Node_Access);
   --  Release the memory associated with the tree in Item

   --
   --  Symbolic_Expression is just a thin "shell" around Node_Access.
   --  This makes recursive procedures a bit simpler and allows us
   --  to hide all the house-keeping from the user.
   --
   type Symbolic_Expression is  new
     Ada.Finalization.Controlled
   with
      record
         Expr : Node_Access;
      end record;

   overriding procedure Initialize (Item : in out Symbolic_Expression);
   overriding procedure Finalize (Item : in out Symbolic_Expression);
   overriding procedure Adjust (Item : in out Symbolic_Expression);



end Symbolic_Expressions;
