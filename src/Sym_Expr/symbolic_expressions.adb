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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;

with Ada.Strings.Unbounded;

package body Symbolic_Expressions is
   --  Set to True to enable few debug prints
   Verbose : constant Boolean := False;
   pragma Unreferenced (Verbose);

   ---------
   -- "+" --
   ---------

   function "+" (L : Symbolic_Expression) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class => Unary_Plus,
                                    Term  => Duplicate (L.Expr)));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Symbolic_Expression) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class => Unary_Minus,
                                    Term  => Duplicate (L.Expr)));
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Symbolic_Expression) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class => Sum,
                                    Left  => Duplicate (L.Expr),
                                    Right => Duplicate (R.Expr)));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Symbolic_Expression) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class => Sub,
                                    Left  => Duplicate (L.Expr),
                                    Right => Duplicate (R.Expr)));
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Symbolic_Expression) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class => Mult,
                                    Left  => Duplicate (L.Expr),
                                    Right => Duplicate (R.Expr)));
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Symbolic_Expression) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class => Div,
                                    Left  => Duplicate (L.Expr),
                                    Right => Duplicate (R.Expr)));
   end "/";

   ---------
   -- "+" --
   ---------

   function "+" (L : Symbolic_Expression; R : Scalar_Type)
                 return Symbolic_Expression
   is
   begin
      return L + To_Expr (R);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Symbolic_Expression; R : Scalar_Type)
                 return Symbolic_Expression
   is
   begin
      return L - To_Expr (R);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L : Symbolic_Expression; R : Scalar_Type)
                 return Symbolic_Expression
   is
   begin
      return L * To_Expr (R);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L : Symbolic_Expression; R : Scalar_Type)
                 return Symbolic_Expression
   is
   begin
      return L / To_Expr (R);
   end "/";

   ---------
   -- "+" --
   ---------

   function "+" (L : Scalar_Type; R : Symbolic_Expression)
                 return Symbolic_Expression
   is
   begin
      return To_Expr (L) + R;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Scalar_Type; R : Symbolic_Expression)
                 return Symbolic_Expression
   is
   begin
      return To_Expr (L) - R;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L : Scalar_Type; R : Symbolic_Expression)
                 return Symbolic_Expression
   is
   begin
      return To_Expr (L) * R;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L : Scalar_Type; R : Symbolic_Expression)
                 return Symbolic_Expression
   is
   begin
      return To_Expr (L) / R;
   end "/";

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (X : Node_Access) return Boolean is
   begin
      case X.Class is
         when Unary_Plus | Unary_Minus =>
            return Is_Constant (X.Term);
         when Sum | Sub | Mult | Div =>
            return Is_Constant (X.Left) and Is_Constant (X.Right);
         when Fun_Call =>

            for I in 1 .. X.N_Params loop
               if not Is_Constant (X.Parameters (I)) then
                  return False;
               end if;
            end loop;

            return True;
         when Var =>
            return False;
         when Const =>
            return True;
      end case;
   end Is_Constant;

   function Is_Constant (X : Symbolic_Expression) return Boolean is
   begin
      return Is_Constant (X.Expr);
   end Is_Constant;

   -------------------
   -- Constant_Expr --
   -------------------

   function To_Expr (X : Scalar_Type) return Symbolic_Expression is
   begin
      return Symbolic_Expression'
         (Controlled with
             Expr => new Node_Type'(Class  => Const,
                                    Value  => X));
   end To_Expr;

   ---------------
   -- To_Scalar --
   ---------------

   function To_Scalar (X : Node_Access) return Scalar_Type is
   begin
      case X.Class is
         when Unary_Plus =>
            return + To_Scalar (X.Term);
         when  Unary_Minus =>
            return - To_Scalar (X.Term);
         when Sum =>
            return To_Scalar (X.Left) + To_Scalar (X.Right);

         when Sub =>
            return To_Scalar (X.Left) - To_Scalar (X.Right);

         when Mult =>
            return To_Scalar (X.Left) * To_Scalar (X.Right);

         when Div =>
            return To_Scalar (X.Left) / To_Scalar (X.Right);

         when Fun_Call =>
            declare
               Param : Scalar_Array (1 .. X.N_Params);
            begin

               for I in Param'Range loop
                  Param (I) := To_Scalar (X.Parameters (I));
               end loop;

               return Call (Identifier(X.Fun_Name), Param);
            end;

         when Var =>
            raise Not_A_Scalar;

         when Const =>
            return X.Value;
      end case;
   end To_Scalar;

   function Eval (X : Symbolic_Expression) return Scalar_Type is
   begin
      return To_Scalar (X.Expr);
   end Eval;

   -------------------
   -- Function_Call --
   -------------------

   function Function_Call
     (Name          : Function_Name;
                           Parameters    : Expression_Array)
      return Symbolic_Expression
   is
      Node : constant Node_Access :=
               new Node_Type'(Class      => Fun_Call,
                              Fun_Name   => Name,
                              N_Params   => Parameters'Length,
                              Parameters => <>);
   begin
      for I in Parameters'Range loop
         Node.Parameters (I) := Duplicate (Parameters (I).Expr);
      end loop;

      return Symbolic_Expression'(Controlled with Expr => Node);
   end Function_Call;

   --------------
   -- Variable --
   --------------

   function Variable
     (Name : Variable_Name) return Symbolic_Expression
   is
   begin
      return Symbolic_Expression'(Controlled with Expr =>
                                  new Node_Type'(Class    => Var,
                                                 Var_Name => Name));
   end Variable;



   function Replace
      (Item     : Node_Access;
       Var_Name : Variable_Name;
       Value    : Node_Access)
       return Node_Access
   is
      function Result_Class (Item     : Node_Access;
                             Var_Name : Variable_Name) return Node_Class is
      begin
         if Item.Class = Var and then Item.Var_Name = Var_Name then
            return Const;
         else
            return Item.Class;
         end if;
      end Result_Class;
      pragma Unreferenced (Result_Class);

      Result : Node_Access;
   begin
      --        Ada.Text_IO.Put_Line ("Bibi" & Item.Class'img);

      case Item.Class is
         when Unary_Plus | Unary_Minus =>
            Result := new Node_Type (Item.Class);
            Result.Term := Replace (Item.Term, Var_Name, Value);

            pragma Assert (Result.Class = Item.Class);

         when Sum | Sub | Mult | Div =>
            Result := new Node_Type (Item.Class);
            Result.Left := Replace (Item.Left, Var_Name, Value);
            Result.Right := Replace (Item.Right, Var_Name, Value);

            pragma Assert (Result.Class = Item.Class);
         when Fun_Call =>
            Result := new Node_Type (Item.Class);
            Result.Fun_Name := Item.Fun_Name;
            Result.N_Params := Item.N_Params;

            for I in 1 .. Result.N_Params loop
               Result.Parameters (I) := Replace (Item.Parameters (I), Var_Name, Value);
            end loop;

            pragma Assert (Result.Class = Fun_Call);
         when Var =>
            --              Ada.Text_Io.Put_Line ("'" & To_String (Var_Name) & "'  '"
            --                                    & To_String (Item.Var_Name) & "'");

            if Item.Var_Name = Var_Name then
               Result := Duplicate (Value);
            else
               Result := Duplicate (Item);
            end if;

         when Const =>
            Result := Duplicate (Item);
      end case;

      --        Ada.Text_IO.Put_Line ("Bobo" & Item.Class'img);
      return Result;
   end Replace;

   -------------
   -- Replace --
   -------------

   function Replace
     (Item     : Symbolic_Expression;
                     Var_Name : Variable_Name;
                     Value    : Symbolic_Expression)
      return Symbolic_Expression
   is
   begin
      return Symbolic_Expression'
        (Controlled with
           Expr => Replace (Item     => Item.Expr,
                            Var_Name => Var_Name,
                            Value    => Value.Expr));
   end Replace;

   function Replace (Item  : Symbolic_Expression;
                     Table : Variable_Tables.Map)
                     return Symbolic_Expression
   is


      Result : Symbolic_Expression := Item;

      procedure Process (Name : Variable_Name) is
         use Variable_Tables;

         Pos : constant Cursor := Table.Find (Name);
      begin
         if Pos = No_Element then
            return;
         else
            Result := Replace (Item     => Result,
                               Var_Name => Name,
                               Value    => Element (Pos));
         end if;
      end Process;
   begin
      Iterate_On_Vars (Item, Process'Access);

      return Result;
   end Replace;


   function Replace (Item     : Symbolic_Expression;
                     Var_Name : Variable_Name;
                     Value    : Scalar_Type)
                     return Symbolic_Expression
   is
   begin
      return Symbolic_Expression'
         (Controlled with
            Expr => Replace (Item.Expr, Var_Name, To_Expr(Value).Expr));
   end Replace;



   function Free_Variables (Item : Symbolic_Expression)
                            return Variable_Sets.Set
   is
      procedure Fill_List (Item  : Node_Access;
                           Names : in out Variable_Sets.Set)
      is
      begin
         case Item.Class is
         when Unary_Plus | Unary_Minus =>
            Fill_List (Item.Term, Names);

         when Sum | Sub | Mult | Div =>
            Fill_List (Item.Left, Names);
            Fill_List (Item.Right, Names);

         when Fun_Call =>

            for I in 1 .. Item.N_Params loop
               Fill_List (Item.Parameters (I), Names);
            end loop;

         when Var =>
            Names.Include (Item.Var_Name);

         when Const =>
            null;

         end case;
      end Fill_List;

      Result : Variable_Sets.Set;
   begin
      Fill_List (Item.Expr, Result);
      return Result;
   end Free_Variables;

   ---------------------
   -- Iterate_On_Vars --
   ---------------------

   procedure Iterate_On_Vars
     (Item : Symbolic_Expression;
      Process : access procedure (Var_Name : Variable_Name))
   is
      use Variable_Sets;

      --        Variables : Var_Lists.Set :=

      procedure Call (Pos : Cursor) is
      begin
         Process (Element (Pos));
      end Call;
   begin
      Free_Variables (Item).Iterate (Call'Access);
   end Iterate_On_Vars;

   ---------------
   -- Normalize --
   ---------------

   --     procedure Normalize (Item : in out Symbolic_Expression) is
   --        pragma Unreferenced (Item);
   --     begin
   --        --  Generated stub: replace with real body!
   --        pragma Compile_Time_Warning (Standard.False, "Normalize unimplemented");
   --        raise Program_Error with "Unimplemented procedure Normalize";
   --     end Normalize;


   function Duplicate (Item : Node_Access) return Node_Access
   is
      Result : constant Node_Access := new Node_Type (Item.Class);
   begin
      if Item = null then
         return null;
      end if;

      case Item.Class is
         when Unary_Plus | Unary_Minus =>
            Result.Term := Duplicate (Item.Term);
         when Sum | Sub | Mult | Div =>
            Result.Left := Duplicate (Item.Left);
            Result.Right := Duplicate (Item.Right);
         when Fun_Call =>
            Result.Fun_Name := Item.Fun_Name;
            Result.N_Params := Item.N_Params;

            for I in 1 .. Result.N_Params loop
               Result.Parameters (I) := Duplicate (Item.Parameters (I));
            end loop;
         when Var =>
            Result.Var_Name := Item.Var_Name;
         when Const =>
            Result.Value := Item.Value;
      end case;

      return Result;
   end Duplicate;

   procedure Free (Item : in out Node_Access)
   is
      procedure Dealloc is
         new Ada.Unchecked_Deallocation (Object => Node_Type,
                                         Name   => Node_Access);
   begin
      if Item = null then
         return;
      end if;

      pragma Assert (Item /= null);

      case Item.Class is
         when Unary_Plus | Unary_Minus =>
            Free (Item.Term);
         when Sum | Sub | Mult | Div =>
            Free (Item.Left);
            Free (Item.Right);
         when Fun_Call =>
            for I in 1 .. Item.N_Params loop
               Free (Item.Parameters (I));
            end loop;
         when Var =>
            null;
         when Const =>
            null;
      end case;

      Dealloc (Item);
   end Free;

   function Dump (Item      : Node_Access;
                  Level     : Natural)
                  return String
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      function Head (X : String) return Unbounded_String is
      begin
         return To_Unbounded_String (((Level * 3) * " ") & X);
      end Head;

      CRLF   : constant String := Character'Val (13) & Character'Val (10);
      Result : Unbounded_String;
   begin
      case Item.Class is
         when Unary_Plus =>
            Result := Head ("@+") & CRLF & Dump (Item.Term, Level + 1);

         when  Unary_Minus =>
            Result := Head ("@-") & CRLF & Dump (Item.Term, Level + 1);

         when Sum =>
            Result := Head ("+")
               & CRLF & Dump (Item.Left, Level + 1)
               & CRLF & Dump (Item.Right, Level + 1);

         when Sub =>
            Result := Head ("-")
               & CRLF & Dump (Item.Left, Level + 1)
               & CRLF & Dump (Item.Right, Level + 1);

         when Mult =>
            Result := Head ("*")
               & CRLF & Dump (Item.Left, Level + 1)
               & CRLF & Dump (Item.Right, Level + 1);

         when Div =>
            Result := Head ("/")
               & CRLF & Dump (Item.Left, Level + 1)
               & CRLF & Dump (Item.Right, Level + 1);

         when Fun_Call =>
            Result := Head ("Call " & ID_Image (Item.Fun_Name));

            for I in 1 .. Item.N_Params loop
               Result := Result & CRLF & Dump (Item.Parameters (I), Level + 1);
            end loop;

         when Var =>
            Result := Head ("Var ") & "(" & ID_Image (Item.Var_Name) & ")";

         when Const =>
            Result := Head ("Const ") & Image (Item.Value);
      end case;

      return To_String (Result);
   end Dump;


   function Dump (Item : Symbolic_Expression) return String
   is
   begin
      return Dump (Item.Expr, 0);
   end Dump;

   overriding procedure Initialize (Item : in out Symbolic_Expression)
   is
   begin
      Item.Expr := null;
   end Initialize;

   overriding procedure Finalize (Item : in out Symbolic_Expression)
   is
   begin
      if Item.Expr /= null then
         Free (Item.Expr);
      end if;
   end Finalize;

   overriding procedure Adjust (Item : in out Symbolic_Expression)
   is
   begin
      if Item.Expr /= null then
         Item.Expr := Duplicate (Item.Expr);
      end if;
   end Adjust;







end Symbolic_Expressions;
