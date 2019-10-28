with Ada.Containers.Indefinite_Ordered_Maps;

generic
   with procedure Read_Scalar (Input    : in     String;
                               Success  :    out Boolean;
                               Consumed :    out Natural;
                               Result   :    out Scalar_Type);
   --  If Input begins with a valid scalar value, set Success to True,
   --  Result to the corresponding scalar value and Consumed to the
   --  number of chars that make the text representation of the scalar.
   --  This procedure is used by parse.

   with procedure Read_Identifier (Input    : in     String;
                                   Success  :    out Boolean;
                                   Consumed :    out Natural;
                                   Result   :    out Identifier);

   with function Join (Prefix, Name : Identifier) return Identifier;

package Symbolic_Expressions.Parsing is
   type ID_Table_Type is private;
   --  The parsing function accepts, as an optional parameter, an "ID
   --  table" that specifies if an ID is a variable or a function and,
   --  in the latter case, how many parameters it accepts.
   --
   --  The behaviour of the Parse function in the presence of an ID that
   --  is not in the ID table can be one of the following
   --
   --     * Always accept
   --        (This is default) The ID is considered a variable or a
   --        function according to the presence of a '(' following the ID.
   --
   --     * Always raise an error
   --
   --     * Accept undefined variables, but not undefined functions.
   --
   --  Note that "Always accept" means "Always accept *undefined* IDs."  If,
   --  for example, the ID is registered as a function, but a '(' does not
   --  follow an error is raised.
   --

   Empty_ID_Table : constant ID_Table_Type;

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

   procedure Define_Variable (Container  : in out ID_Table_Type;
                              Name       : Variable_Name);
   --  Declare a variable with the given name

   procedure Define_Function (Container  : in out ID_Table_Type;
                              Name       : Function_Name;
                              N_Params   : Parameter_Count);
   --  Declare a function with the given name, accepting the given number
   --  of parameters.  Examples:
   --
   --     Define_Function(Table, "sin", Exactly(1));
   --     Define_Function(Table, "max", Any_Number);
   --     Define_Function(Table, "rnd", No_Parameter);
   --

   function Is_Acceptable (N_Param : Natural;
                           Limits  : Parameter_Count)
                           return Boolean;
   --  Return True if N_Param lies in Limits


   --  What to do when the parser finds an ID that is not in the
   --  ID_Table given to the parser
   type Unknown_ID_Action_Type is
     (OK,              --  Always accept the ID
      Accept_As_Var,   --  Accept it, only if used as a variable
      Die);            --  Never accept it

   Parsing_Error : exception;



   type Multiple_Match_Action is (Die, Allow, Allow_Unprefixed);

   function Parse (Input             : String;
                   ID_Table          : ID_Table_Type := Empty_ID_Table;
                   On_Unknown_ID     : Unknown_ID_Action_Type := OK;
                   Prefixes          : String := "";
                   Prefix_Separator  : String := ".";
                   On_Multiple_Match : Multiple_Match_Action := Allow_Unprefixed)
                   return Symbolic_Expression;
   --  Parse a string with an expression and return the result.  The grammar
   --  of the expression is the usual one
   --
   --    Expr   = Term *(('+' | '-') Term)
   --    Term   = Fact *(('*' | '/') Fact)
   --    Fact   = [('+' | '-')] Simple
   --    Simple = Id [ '(' List ')' ] | Scalar | '(' Expr ')'
   --    List   = Expr *(',' Expr)
   --
   --  As usual, [...] denote an optional part, *(...) means
   --  one or more repetition of something and '|' means
   --  alternatives.
   --
   --  Note that
   --
   --    * In the production for Simple a single Id (without a
   --      following list) represents a variabile, while an Id with a following
   --      list represents a function call.
   --
   --      (Yes, folks, a call without arguments is something like foo(),
   --      C-like .. . I know, I know, but with this convention parsing is
   --      a bit easier since the difference is implicit in the syntax)
   --
   --    * Id follows the usual name syntax: letters, digits,
   --      underscores and (in addition) the '.', so that "foo.end" is a
   --      valid identifier.  The first char must be a letter.
   --
   --    * The syntax of Scalar is implicitely defined by the function
   --      Read_Scalar used in the instantiation.  In order to avoid the risk
   --      that the grammar above becomes ambiguous, a scalar should not begin
   --      with a letter, a '(' or a sign.
   --
   --    * If Prefixes is not empty, it is expected to be a sequence of
   --      identifiers separated by spaces.  If an identifier is not found
   --      "as it is," it is searched by prefixing it with the prefixes
   --      in the order given.  For example, if
   --
   --              Prefixes="Ada.Strings Ada.Foo Ada"
   --
   --      and the identifier is "bar," the following strings are searched
   --      in order
   --
   --              Bar
   --              Ada.Strings.Bar
   --              Ada.Foo.Bar
   --              Ada.Bar
   --
   --      It is possible to change the separator used between the identifier
   --      and the prefix by specifyint Prefix_Separator.  For example, if
   --      Prefix_Separator = "::" the following identifiers are searched
   --      for
   --
   --              Bar
   --              Ada.Strings::Bar
   --              Ada.Foo::Bar
   --              Ada::Bar
   --
   --   * If Prefixes is not empty, all prefixes are tried.  The behaviour
   --     used when more than one prefix matches depends on
   --     On_Multiple_Match
   --
   --       - if On_Multiple_Match = Die,
   --            an exception is always raised
   --
   --       - if On_Multiple_Match = Allow,
   --            the first match is taken
   --
   --       - if On_Multiple_Match = Allow_Unprefixed,
   --             if the unprefixed name matches, the unprefixed name
   --             is taken, otherwise an exception is raised
private

   type Parameter_Count is
      record
         Min : Natural;
         Max : Natural;
      end record;


   Any_Number : constant Parameter_Count := (Natural'First, Natural'Last);

   No_Parameter : constant Parameter_Count := (0, 0);

   type ID_Class is (Funct, Var);

   type ID_Descriptor (Class : ID_Class) is
      record
         case Class is
            when Funct =>
               N_Param : Parameter_Count;
            when Var =>
               null;
         end case;
      end record;

   package ID_Tables is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Identifier,
        Element_Type => ID_Descriptor);

   type ID_Table_Type is
      record
         T : ID_Tables.Map := ID_Tables.Empty_Map;
      end record;


   Empty_ID_Table : constant ID_Table_Type := (T => ID_Tables.Empty_Map);

end Symbolic_Expressions.Parsing;
