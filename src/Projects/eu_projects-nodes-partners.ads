with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces;

with EU_Projects.Node_Tables;


--
-- A partner descriptor keeps some basic information about the
-- partner such as
--
--  * The name (a short version, too)
--  * A label
--  * An index
--
-- Moreover, every partner has
--
--  * One or more roles (name, description and PM cost)
--  * One or more expenses
--
-- Expenses can be divisible or undivisible.  Divisible expenses are related
-- to a collections of goods/services where one pays a price per unit.
-- For example, the expense for buying one or more cards is divisible.
-- An example of undivisible expense is the expense for a single service.
-- Honestly, the distintion is quite fuzzy (it is an undivisible expense or
-- it is a divisble one with just one object?), but in some context it
-- could be handy to be able to do this distinction.
--
-- It is possible to iterate over all the roles and all the expenses using
-- All_Expenses and All_Roles.
--
package EU_Projects.Nodes.Partners is
   type Partner (<>) is new Nodes.Node_Type with private;
   type Partner_Access is access all Partner;

   subtype Partner_Index is Node_Index;
   No_Partner : constant Extended_Node_Index := No_Index;

   type Partner_Label is new Node_Label;

   type Partner_Name_Array is array (Partner_Index range <>) of Partner_Label;

   type Role_Name is new Dotted_Identifier;

   subtype Country_Code is String (1 .. 2)
         with Dynamic_Predicate => (for all C of Country_Code => C in 'A' .. 'Z');

   function Create (ID         : Partner_Label;
                    Name       : String;
                    Short_Name : String;
                    Country    : Country_Code;
                    Node_Dir   : in out Node_Tables.Node_Table)
                    return Partner_Access;

   procedure Set_Index (Item : in out Partner;
                        Idx  : Partner_Index);

   function Country (Item : Partner) return Country_Code;

   function Dependency_List (Item : partner)
                             return Node_Label_Lists.Vector
   is (Node_Label_Lists.Empty_Vector);

   overriding function Full_Index (Item     : Partner;
                                   Prefixed : Boolean) return String;

   procedure Add_Role (To           : in out Partner;
                       Role         : in Role_Name;
                       Description  : in String;
                       Monthly_Cost : in Currency);

   procedure Add_Undivisible_Expense (To          : in out Partner;
                                      Description : in String;
                                      Cost        : in Currency);

   procedure Add_Divisible_Expense (To          : in out Partner;
                                    Description : in String;
                                    Unit_Cost   : in Currency;
                                    Amount      : in Natural);


   type Cursor is private;

   function Has_Element (X : Cursor) return Boolean;


   package Expense_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Cursor,
                                  Has_Element  => Has_Element);

   function All_Expenses
     (Item : Partner)
      return Expense_Iterator_Interfaces.Forward_Iterator'Class;

   function Is_Divisible (Pos : Cursor) return Boolean;

   function Unit_Cost (Pos : Cursor) return Currency
     with
       Pre => Is_Divisible (Pos);

   function Amount (Pos : Cursor) return Natural
     with
       Pre => Is_Divisible (Pos);

   function Total_Cost (Pos : Cursor) return Currency;

   function Description (Pos : Cursor) return String;

   type Role_Cursor is private;


   function Has_Element (X : Role_Cursor) return Boolean;


   package Role_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Role_Cursor,
                                  Has_Element  => Has_Element);

   function All_Roles
     (Item : Partner)
      return Role_Iterator_Interfaces.Forward_Iterator'Class;


   function Cost (Pos : Role_Cursor) return Currency;

   function Description (Pos : Role_Cursor) return String;

   function Name (Pos : Role_Cursor) return String;

   pragma Warnings (Off);
   overriding function Get_Symbolic_Instant
     (X   : Partner;
      Var : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Instant
   is (raise Unknown_Instant_Var);

   overriding function Get_Symbolic_Duration
     (X   : Partner;
      Var : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Duration
   is (raise Unknown_Duration_Var);

   pragma Warnings (On);
private
   type Personnel_Cost is
      record
         Role         : Role_Name;
         Monthly_Cost : Currency;
         Description  : Unbounded_String;
      end record;



   type Expense (Divisible : Boolean := False)  is
      record
         Description : Unbounded_String;

         case Divisible is
            when True =>
               Unit_Cost : Currency;
               Amount    : Natural;

            when False =>
               Cost      : Currency;
         end case;
      end record;

   package Personnel_Cost_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Personnel_Cost);

   package Other_Cost_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Expense);

   type Partner is new Nodes.Node_Type
   with
      record
         Personnel_Costs : Personnel_Cost_Vectors.Vector;
         Other_Costs     : Other_Cost_Vectors.Vector;
         Country         : Country_Code;
      end record;

   function Country (Item : Partner) return Country_Code
   is (Item.Country);

   type Cursor is
      record
         Pos : Other_Cost_Vectors.Cursor;
      end record;

   type Role_Cursor is
      record
         Pos : Personnel_Cost_Vectors.Cursor;
      end record;

   type Expense_Iterator is
     new Expense_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Other_Cost_Vectors.Cursor;
      end record;

   overriding
   function Full_Index (Item     : Partner;
                        Prefixed : Boolean) return String
   is (Image (Item.Index));


   overriding
   function First (Object : Expense_Iterator) return Cursor
   is (Cursor'(Pos => Object.Start));

   overriding
   function Next
     (Object   : Expense_Iterator;
      Position : Cursor) return Cursor
   is (Cursor'(Pos => Other_Cost_Vectors.Next (Position.Pos)));

   function All_Expenses
     (Item : Partner)
      return Expense_Iterator_Interfaces.Forward_Iterator'Class
   is (Expense_Iterator'(Expense_Iterator_Interfaces.Forward_Iterator
                         with
                           Start => Item.Other_Costs.First));

   function Has_Element (X : Cursor) return Boolean
   is (Other_Cost_Vectors.Has_Element (X.Pos));

   function Is_Divisible (Pos : Cursor) return Boolean
   is (Other_Cost_Vectors.Element (Pos.Pos).Divisible);

   function Unit_Cost (Pos : Cursor) return Currency
   is (Other_Cost_Vectors.Element (Pos.Pos).Unit_Cost);

   function Total_Cost (Pos : Cursor) return Currency
   is (
       if Is_Divisible (Pos)
       then
          Unit_Cost (Pos) * Amount (Pos)
       else
          Other_Cost_Vectors.Element (Pos.Pos).Cost
      );


   function Amount (Pos : Cursor) return Natural
   is (Other_Cost_Vectors.Element (Pos.Pos).Amount);

   function Description (Pos : Cursor) return String
   is (To_String (Other_Cost_Vectors.Element (Pos.Pos).Description));


   type Role_Iterator is
     new Role_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Personnel_Cost_Vectors.Cursor;
      end record;

   function First (Object : Role_Iterator) return Role_Cursor
   is (Role_Cursor'(Pos => Object.Start));

   overriding
   function Next
     (Object   : Role_Iterator;
      Position : Role_Cursor) return Role_Cursor
   is (Role_Cursor'(Pos => Personnel_Cost_Vectors.Next (Position.Pos)));

   function Has_Element (X : Role_Cursor) return Boolean
   is (Personnel_Cost_Vectors.Has_Element (X.Pos));

   function All_Roles
     (Item : Partner)
      return Role_Iterator_Interfaces.Forward_Iterator'Class
   is (Role_Iterator'(Role_Iterator_Interfaces.Forward_Iterator
                      with
                        Start => Item.Personnel_Costs.First));

   function Cost (Pos : Role_Cursor) return Currency
   is (Personnel_Cost_Vectors.Element (Pos.Pos).Monthly_Cost);

   function Description (Pos : Role_Cursor) return String
   is (To_String (Personnel_Cost_Vectors.Element (Pos.Pos).Description));

   function Name (Pos : Role_Cursor) return String
   is (Image (Personnel_Cost_Vectors.Element (Pos.Pos).Role));
end EU_Projects.Nodes.Partners;
