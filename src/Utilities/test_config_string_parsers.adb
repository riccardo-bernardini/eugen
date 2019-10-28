with Config_String_Parsers;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Config_String_Parsers is
   function Identity (X : String) return String
   is (X);

   package Test_Parsers is
     new Config_String_Parsers (Name_Type   => string,
                                Value_Type  => String,
                                No_Value    => "(null)",
                                To_Name     => Identity,
                                To_Value    => Identity);

   procedure Dump (Y : Test_Parsers.Parsing_Result) is
      use Test_Parsers.Parameter_Maps, Test_Parsers;

      X : constant Test_Parsers.Parameter_Maps.Map := Parameters (Y);
   begin
      Put_Line ("##################");
      Put_line ("header: '"
                & (if Has_Header (Y) then Header (Y) else "")
                & "'");

      for Pos in X.Iterate loop
         Put_Line ("'" & Key (Pos) & "' -> '" & Element (Pos) & "'");
      end loop;
   end Dump;

   Params : Test_Parsers.Parsing_Result;

begin
   Params := Test_Parsers.Parse (" ciao , pluto= 33 ,zimo-xamo={  42,33 }");
   Dump (Params);

   Params := Test_Parsers.Parse
     (Input   => "zorro : ciao , pluto= 33 ,zimo-xamo={}",
      Options => Test_Parsers.Config (Expect_Header => Test_Parsers.Maybe));
   Dump (Params);

   declare
      use Test_Parsers;
      Syntax : Syntax_Descriptor;
   begin
      Add_Parameter_Syntax (Syntax         => Syntax,
                            Parameter_Name => "ciao",
                            If_Missing     => die);

      Add_Parameter_Syntax(Syntax         => Syntax,
                           Parameter_Name => "pippo",
                           If_Missing     => Use_Default,
                           Default        => "minima veniali");

      Params := Test_Parsers.Parse
        (Input           => "zorro : ciao , pluto=,zimo-xamo={  42,33 }",
         Syntax          => Syntax,
         Options         => Test_Parsers.Config (Expect_Header => Test_Parsers.Maybe),
         On_Unknown_Name => OK);
      Dump (Params);

      Params := Test_Parsers.Parse
        (Input           => "zorro : ciao , pippo= 37 ,zimo-xamo={  42,33 }",
         Syntax          => Syntax,
         Options         => Test_Parsers.Config (Expect_Header => Test_Parsers.Maybe),
         On_Unknown_Name => OK);
      Dump (Params);

      Params := Test_Parsers.Parse
        (Input           => "zorro : ciao=112, pippo={33,11},zimo--xamo={  42,33 }",
         Syntax          => Syntax,
         Options         => Test_Parsers.Config (Expect_Header => Test_Parsers.Maybe),
         On_Unknown_Name => OK);
      Dump (Params);
   end;
end Test_Config_String_Parsers;
