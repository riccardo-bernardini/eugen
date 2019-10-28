with Ada.Containers.Indefinite_Ordered_Maps;

with Symbolic_Expressions.Solving;

package  EU_Projects.Times.Time_Expressions.Solving is
   type Time_Equation_System is private;


   function Contains_Left_Term (Equations : Time_Equation_System;
                                Left      : Dotted_Identifier)
                                   return Boolean;



   procedure Add_Equation (Equations : in out Time_Equation_System;
                           Left      : Dotted_Identifier;
                           Right     : Symbolic_Duration)
     with
       Pre => not Contains_Left_Term (Equations, Left),
     Post => Contains_Left_Term (Equations, Left);

   procedure Add_Equation (Equations : in out Time_Equation_System;
                           Left      : Dotted_Identifier;
                           Right     : Symbolic_Instant)
     with
       Pre => not Contains_Left_Term (Equations, Left),
     Post => Contains_Left_Term (Equations, Left);


   type Variable_Map is tagged private
     with Constant_Indexing => Get;

   function Contains (Map : Variable_Map;
                      ID  : Dotted_Identifier)
                      return Boolean;

   function Value_Class (Map : Variable_Map;
                         Id  : Dotted_Identifier)
                         return Time_Type
     with Pre => Contains (Map, Id);

   function Get (Map : Variable_Map;
                 ID  : Dotted_Identifier)
                 return Duration
     with
       Pre => Contains (Map, ID) and then Value_Class (Map, Id) = Duration_Value;

   function Get (Map : Variable_Map;
                 ID  : Dotted_Identifier)
                 return Instant
     with
       Pre => Contains (Map, ID) and then Value_Class (Map, Id) = Instant_Value;

   function Solve (Equations : Time_Equation_System) return Variable_Map;
   Unsolvable : exception;


private
     package Time_Equations is new Time_Expr.Solving;


   type Equation_Right_Side is
      record
         Class : Time_Type;
         Val   : Time_Expr.Symbolic_Expression;
      end record;

   package Equation_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Dotted_Identifier,
        Element_Type => Equation_Right_Side);

   type Time_Equation_System is
      record
         M : Equation_Maps.Map;
      end record;


   type Variable_Value (Class : Time_Type) is
      record
         case Class is
            when Instant_Value =>
               I : Instant;

            when Duration_Value =>
               D : Duration;
         end case;
      end record;

   package Variable_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Dotted_Identifier,
        Element_Type => Variable_Value);

   type Variable_Map is tagged
      record
         M : Variable_Maps.Map;
      end record;

   function Contains (Map : Variable_Map;
                      ID  : Dotted_Identifier)
                      return Boolean
   is (Map.M.Contains (ID));

    function Value_Class (Map : Variable_Map;
                         Id  : Dotted_Identifier)
                          return Time_Type
   is (Map.M (Id).Class);

   function Get (Map : Variable_Map;
                 ID  : Dotted_Identifier)
                 return Duration
   is (Map.M.Element (ID).D);

   function Get (Map : Variable_Map;
                 ID  : Dotted_Identifier)
                 return Instant
   is (Map.M.Element (ID).I);


   function Contains_Left_Term (Equations : Time_Equation_System;
                                Left      : Dotted_Identifier)
                                return Boolean
   is (Equations.M.Contains (Left));


end EU_Projects.Times.Time_Expressions.Solving;
