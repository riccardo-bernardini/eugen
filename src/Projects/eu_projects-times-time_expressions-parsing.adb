pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body EU_Projects.Times.Time_Expressions.Parsing is

   ------------------------
   -- Fill_With_Defaults --
   ------------------------

   procedure Fill_With_Defaults (Container : in out Symbol_Table)
   is
   begin
      Define_Function (Container => Container,
                       Name      => To_Id ("max"),
                       N_Params  => At_Least (1));

      Define_Function (Container => Container,
                       Name      => To_Id ("min"),
                       N_Params  => At_Least (1));

      Define_Function (Container => Container,
                       Name      => To_Id ("lt"),
                       N_Params  => Exactly (2));

      Define_Function (Container => Container,
                       Name      => To_Id ("le"),
                       N_Params  => Exactly (2));

      Define_Function (Container => Container,
                       Name      => To_Id ("gt"),
                       N_Params  => Exactly (2));

      Define_Function (Container => Container,
                       Name      => To_Id ("ge"),
                       N_Params  => Exactly (2));

      Define_Function (Container => Container,
                       Name      => To_Id ("eq"),
                       N_Params  => Exactly (2));

      Define_Function (Container => Container,
                       Name      => To_Id ("ne"),
                       N_Params  => Exactly (2));

      Define_Function (Container => Container,
                       Name      => To_Id ("if"),
                       N_Params  => Exactly (3));
   end Fill_With_Defaults;

   ---------------------
   -- Define_Variable --
   ---------------------

   procedure Define_Variable
     (Container : in out Symbol_Table; Name : Simple_Identifier)
   is
   begin
      Internal_Parsing.Define_Variable (Container => Container.T,
                                        Name      => Name);
   end Define_Variable;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Container : in out Symbol_Table; Name : Simple_Identifier;
      N_Params  :        Parameter_Count)
   is
   begin
      Internal_Parsing.Define_Function (Container => Container.T,
                                        Name      => Name,
                                        N_Params  => N_Params.C);
   end Define_Function;

   -----------------
   -- Read_Scalar --
   -----------------


   procedure Read_Scalar (Input    : in     String;
                          Success  :    out Boolean;
                          Consumed :    out Natural;
                          Result   :    out Scalar_Type)
   is

      procedure Get_tbd (X     : String;
                         First : out Natural;
                         Last  : out Natural;
                         Found : out Boolean)
      is
         use Ada.Strings.Fixed;

         Index : constant Natural := Index_Non_Blank (X);
      begin
         if (Index + 2 <= X'Last)
           and then To_Upper (X (Index .. Index + 2)) = "TBD"
         then
            First := Index;
            Last := Index + 2;
            Found := Standard.True;

         else
            First := 0;
            Last := 0;
            Found := False;
         end if;
      end Get_tbd;
      -------------
      -- Get_Int --
      -------------

      procedure Get_Int (X     : String;
                         First : out Natural;
                         Last  : out Natural;
                         Found : out Boolean)
      is
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps;

         Index : constant Natural := Index_Non_Blank (X);

         Int_Chars : constant Character_Set := To_Set (Character_Range'('0', '9'));
      begin
         --        Ada.Text_Io.Put_Line ("Called [" & X & "]");

         if Index = 0 or else not  Is_In (X (Index), Int_Chars) then
            First := 0;
            Last := 0;
            Found := False;
            return;
         end if;

         Found := Standard.True;

         Find_Token (Source => X (Index .. X'Last),
                     Set    => Int_Chars,
                     Test   => Ada.Strings.Inside,
                     First  => First,
                     Last   => Last);

         --  Last should not be zero since we checked that X(Index) is a digit
         pragma Assert (Last /= 0);

      end Get_Int;

      First : Natural;
      Last  : Natural;
      Found : Boolean;
   begin
      Get_Int (Input, First, Last, Found);

      if Found then
         Consumed := Last - Input'First + 1;
         Success := Standard.True;
         -- Without any specifier, the time is in months
         Result := Scalar_Type (Scalar (Integer'Value (Input (First .. Last)) * 4));
         return;
      end if;


      Get_Tbd (Input, First, Last, Found);

      if Found then
         Consumed := Last - Input'First + 1;
         Success := Standard.True;

         Result := TBD;
         return;
      end if;


      Consumed := 0;
      Success := False;
   end Read_Scalar;

end EU_Projects.Times.Time_Expressions.Parsing;
