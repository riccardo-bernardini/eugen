with Symbolic_Expressions;

package EU_Projects.Times.Time_Expressions is
   type Symbolic_Duration is private;
   type Symbolic_Instant is private;
   type Symbolic_Instant_Array is array (Positive range <>) of Symbolic_Instant;


   function Min (L, R : Symbolic_Instant) return Symbolic_Instant;
   function Max (L, R : Symbolic_Instant) return Symbolic_Instant;

   function Min (X : Symbolic_Instant_Array) return Symbolic_Instant;
   function Max (X : Symbolic_Instant_Array) return Symbolic_Instant;

   function Min (L, R : Symbolic_Duration) return Symbolic_Duration;
   function Max (L, R : Symbolic_Duration) return Symbolic_Duration;

   function "-" (L, R : Symbolic_Instant) return Symbolic_Duration;
   function "+" (L : Symbolic_Instant; R : Symbolic_Duration) return Symbolic_Instant;
   function "-" (L : Symbolic_Instant; R : Symbolic_Duration) return Symbolic_Instant;
   function "+" (L : Symbolic_Duration; R : Symbolic_Instant) return Symbolic_Instant;

   function Variable (Name : Dotted_Identifier) return Symbolic_Instant;
   function Variable (Name : Dotted_Identifier) return Symbolic_Duration;

   function Is_Constant (Item : Symbolic_Instant) return Boolean;
   function Is_Constant (Item : Symbolic_Duration) return Boolean;

   function To_Scalar (Item : Symbolic_Instant) return Instant;
   function To_Scalar (Item : Symbolic_Duration) return Duration;

   function Symbolic (Item : Instant) return Symbolic_Instant;
   function Symbolic (Item : Duration) return Symbolic_Duration;



   function Dump(X : Symbolic_Instant) return String;
private
   function Call (Name : Dotted_Identifier; Param : Scalar_Array)
                  return Scalar_Type;


   package Time_Expr is
     new Symbolic_Expressions (Scalar_Type  => Scalar_Type,
                               Scalar_Array => Scalar_Array,
                               Identifier    => Dotted_Identifier,
                               Image         => Image,
                               ID_Image      => Image);

   use type Time_Expr.Symbolic_Expression;




   type Symbolic_Instant is
      record
         T : Time_Expr.Symbolic_Expression;
      end record;

   type Symbolic_Duration is
      record
         D : Time_Expr.Symbolic_Expression;
      end record;

   function Dump (X : Symbolic_Instant) return String
     is (Time_Expr.Dump(X.T));
   Min_Function : constant Dotted_Identifier := To_ID ("min");
   Max_Function : constant Dotted_Identifier := To_ID ("max");

   function Variable (Name : Dotted_Identifier) return Symbolic_Instant
   is (Symbolic_Instant'(T => Time_Expr.Variable (Name)));

   function Variable (Name : Dotted_Identifier) return Symbolic_Duration
   is (Symbolic_Duration'(D => Time_Expr.Variable (Name)));

   function Min (L, R : Symbolic_Instant) return Symbolic_Instant
   is (T => Time_Expr.Function_Call (Min_Function,  (L.T, R.T)));

   function Max (L, R : Symbolic_Instant) return Symbolic_Instant
   is (T => Time_Expr.Function_Call (Max_Function,  (L.T, R.T)));

   function Min (L, R : Symbolic_Duration) return Symbolic_Duration
   is (D => Time_Expr.Function_Call (Min_Function,  (L.D, R.D)));

   function Max (L, R : Symbolic_Duration) return Symbolic_Duration
   is (D => Time_Expr.Function_Call (Max_Function,  (L.D, R.D)));

   function "-" (L, R : Symbolic_Instant) return Symbolic_Duration
   is (D => L.T - R.T);

   function "+" (L : Symbolic_Instant; R : Symbolic_Duration) return Symbolic_Instant
   is (T => L.T + R.D);

   function "-" (L : Symbolic_Instant; R : Symbolic_Duration) return Symbolic_Instant
   is (T => L.T - R.D);

   function "+" (L : Symbolic_Duration; R : Symbolic_Instant) return Symbolic_Instant
   is (T => L.D + R.T);


   function Is_Constant (Item : Symbolic_Instant) return Boolean
   is (Item.T.Is_Constant);

   function Is_Constant (Item : Symbolic_Duration) return Boolean
   is (Item.D.Is_Constant);




   function To_Scalar (Item : Symbolic_Instant) return Instant
   is (Instant (Time_Expr.Eval(Item.T)));

   function To_Scalar (Item : Symbolic_Duration) return Duration
   is (Duration (Time_Expr.Eval(Item.D)));


   function Symbolic (Item : Instant) return Symbolic_Instant
   is (T => Time_Expr.To_Expr (Scalar_Type (Item)));

   function Symbolic (Item : Duration) return Symbolic_Duration
   is (D => Time_Expr.To_Expr (Scalar_Type (Item)));


end EU_Projects.Times.Time_Expressions;
