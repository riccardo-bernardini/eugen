with Symbolic_Expressions.Parsing;

package EU_Projects.Times.Time_Expressions.Parsing is
   type Symbol_Table is private;

    type Parameter_Count is private;
   --  A Parameter_Count stores the spec of a function in terms of number
   --  of accepted parameters.  All the combinations are possible: no
   --  parameter, an exact number of parameters, a range or any number of
   --  parameters.  To create a Parameter_Count you can use the constructors
   --  Exactly, At_Least and Between or use the constants Any_Number or
   --  No_Parameter.
   --

   function Exactly  (N : Natural) return Parameter_Count;
   function At_Least (N : Natural) return Parameter_Count;
   function Between (Min, Max : Natural) return Parameter_Count;

   Any_Number   : constant Parameter_Count;
   No_Parameter : constant Parameter_Count;

   procedure Fill_With_Defaults (Container : in out Symbol_Table);

   procedure Define_Variable (Container  : in out Symbol_Table;
                              Name       : Simple_Identifier);
   --  Declare a variable with the given name

   procedure Define_Function (Container  : in out Symbol_Table;
                              Name       : Simple_Identifier;
                              N_Params   : Parameter_Count);

   function Parse (Input   : String;
                   Symbols : Symbol_Table) return Symbolic_Instant;

   function Parse (Input   : String;
                   Symbols : Symbol_Table) return Symbolic_Duration;
private
   procedure Read_Scalar (Input    : in     String;
                          Success  :    out Boolean;
                          Consumed :    out Natural;
                          Result   :    out Scalar_Type);


   package Internal_Parsing is
     new Time_Expr.Parsing (Read_Scalar     => Read_Scalar,
                            Read_Identifier => ID_Readers.Reader,
                            Join            => Join);

   type Symbol_Table is
      record
         T : Internal_Parsing.ID_Table_Type;
      end record;

   type Parameter_Count is
      record
         C : Internal_Parsing.Parameter_Count;
      end record;

   function Exactly  (N : Natural) return Parameter_Count
   is (C => Internal_Parsing.Exactly (N));

   function At_Least (N : Natural) return Parameter_Count
   is (C => Internal_Parsing.At_Least (N));

   function Between (Min, Max : Natural) return Parameter_Count
   is (C => Internal_Parsing.Between (Min, max));


   Any_Number   : constant Parameter_Count := (C => Internal_Parsing.Any_Number);
   No_Parameter : constant Parameter_Count := (C => Internal_Parsing.No_Parameter);

   function Parse (Input   : String;
                   Symbols : Symbol_Table) return Symbolic_Instant
   is (T => Internal_Parsing.Parse (Input             => Input,
                                    ID_Table          => Symbols.T,
                                    On_Unknown_ID     => Internal_Parsing.Die,
                                    On_Multiple_Match => Internal_Parsing.Die));

   function Parse (Input   : String;
                   Symbols : Symbol_Table) return Symbolic_Duration
   is (D => Internal_Parsing.Parse (Input             => Input,
                                    ID_Table          => Symbols.T,
                                    On_Unknown_ID     => Internal_Parsing.Die,
                                    On_Multiple_Match => Internal_Parsing.Die));


end EU_Projects.Times.Time_Expressions.Parsing;
