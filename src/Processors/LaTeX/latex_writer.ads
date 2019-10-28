with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Latex_Writer is
   type Style_Spec (<>) is private;
   type Table_Handler (<>) is tagged private;

   type Length_Unit is (Pt, Cm, Mm, Inch);
   type Latex_Length is private;

   subtype Em_Length is Float;

   Zero : constant Latex_Length;

   function "+" (X, Y : Latex_Length) return Latex_Length;
   function "-" (X, Y : Latex_Length) return Latex_Length;
   function "*" (X : Float;  Y : Latex_Length) return Latex_Length;
   --     function "*" (X : Em_Length;  Y : Latex_Length) return Latex_Length;
   function Value (X : Latex_Length; Unit : Length_Unit) return Float;

   function Image (X    : Latex_Length;
                   Unit : Length_Unit := Pt;
                   Full : Boolean := True)
                   return String;

   function "/" (X : Latex_Length;  Y : Latex_Length) return Float;

   function Inch return Latex_Length;
   function Pt return Latex_Length;
   function Cm return Latex_Length;
   function Mm return Latex_Length;

   type Parameter_List (<>) is private;

   function "and" (X, Y : String) return Parameter_List;
   function "and" (X : Parameter_List; Y : String) return Parameter_List;

   procedure Within
         (Output    : File_Access;
          Env_Name  : String;
          Callback  : access procedure (Output : File_Access);
          Parameter : String := "");

   procedure Within_Table_Like
         (Output    : File_Access;
          Env_Name  : String;
          Callback  : access procedure (Output : File_Access;
                                        Table : in out Table_Handler));

   procedure Within_Table_Like
         (Output    : File_Access;
          Env_Name  : String;
          Callback  : access procedure (Output : File_Access;
                                        Table : in out Table_Handler);
          Parameter : String);

   procedure Within_Table_Like
         (Output     : File_Access;
          Env_Name   : String;
          Callback   : access procedure (Output : File_Access;
                                         Table : in out Table_Handler);
          Parameters : Parameter_List);


   procedure Within_Table
         (Output        : File_Access;
          Table_Spec    : String;
          Callback      : access procedure (Output : File_Access;
                                            Table : in out Table_Handler);
          Default_Style : String := "";
          Default_Head  : String := "";
          Caption       : String := "";
          Width         : String := "\textwidth");

   procedure Put (Table   : in out Table_Handler;
                  Content : String;
                  Style   : String := "");

   type Bar_Position is (Default, Left, Right, Both, None);

   procedure Head (Table   : in out Table_Handler;
                   Content : String;
                   Bar     : Bar_Position := Default;
                   Style   : String := "");

   procedure New_Row (Table : in out Table_Handler);

   procedure Hline (Table : in out Table_Handler;
                    Full  : Boolean := True);

   procedure Cline (Table : in out Table_Handler; From, To : Positive);

   procedure Multicolumn (Table   : in out Table_Handler;
                          Span    : Positive;
                          Spec    : String;
                          Content : String);

   procedure Print_Default_Macro (Output        : File_Access;
                                  Command_Name  : String;
                                  Definition    : String;
                                  N_Parameters  : Natural);

   type Align_Type is (Center, Left, Right);
   function Hbox (Content : String;
                  Size    : Latex_Length := Zero;
                  Align   : Align_Type := Center)
                  return String;
private
   -- 1 Cm ~ 30 Pt, 10_000 Pt ~ 3m
   type Basic_Latex_Length_Pt is delta 2.0 ** (-16) range -10_000.0 .. 10_000.0;

   type Latex_Length is new Basic_Latex_Length_Pt;

   Zero : constant Latex_Length := 0.0;

   pragma Warnings (Off, "static fixed-point value is not a multiple of Small");

   Point_Per_Unit : constant array (Length_Unit) of Basic_Latex_Length_Pt :=
                      (Pt   => 1.0,
                       Mm   => 2.84,
                       Cm   => 28.4,
                       Inch => 72.27);

   subtype Unit_Name is String (1 .. 2);

   Unit_Image : constant array (Length_Unit) of Unit_Name :=
                  (Pt   => "pt",
                   Mm   => "mm",
                   Cm   => "cm",
                   Inch => "in");

   function "+" (X, Y : Latex_Length) return Latex_Length
   is (Latex_Length (Basic_Latex_Length_Pt (X)+Basic_Latex_Length_Pt (Y)));

   function "-" (X, Y : Latex_Length) return Latex_Length
   is (Latex_Length (Basic_Latex_Length_Pt (X)-Basic_Latex_Length_Pt (Y)));

   function "*" (X : Float;  Y : Latex_Length) return Latex_Length
   is (Latex_Length (X * Float (Y)));

   --     function "*" (X : Em_Length;  Y : Latex_Length) return Latex_Length
   --     is (Float (X) * Y);


   function "/" (X : Latex_Length;  Y : Latex_Length) return Float
   is (Float (X) / Float (Y));

   function Value (X : Latex_Length; Unit : Length_Unit) return Float
   is (Float (X) / Float (Point_Per_Unit (Unit)));

   function Image (X    : Latex_Length;
                   Unit : Length_Unit := Pt;
                   Full : Boolean := True)
                   return String
   is (Basic_Latex_Length_Pt'Image (Basic_Latex_Length_Pt (X) / Point_Per_Unit (Unit))
       & (if Full then Unit_Image (Unit) else ""));

   function Inch return Latex_Length
   is (Latex_Length (Point_Per_Unit (Inch)));

   function Pt return Latex_Length
   is (Latex_Length (Point_Per_Unit (Pt)));

   function Cm return Latex_Length
   is (Latex_Length (Point_Per_Unit (Cm)));

   function Mm return Latex_Length
   is (Latex_Length (Point_Per_Unit (Mm)));


   type Style_Spec is new String;

   type Table_State is (Begin_Row, Middle_Row);

   type Table_Handler is tagged
      record
         State         : Table_State;
         Output        : File_Access;
         Default_Style : Unbounded_String;
         Default_Head  : Unbounded_String;
      end record;

   type Parameter_List is array (Positive range <>) of Unbounded_String;

   function "and" (X, Y : String) return Parameter_List
   is (Parameter_List'(1 => To_Unbounded_String (X),
                       2 => To_Unbounded_String (Y)));

   function "and" (X : Parameter_List; Y : String) return Parameter_List
   is (X & Parameter_List'(1 => To_Unbounded_String (Y)));

end Latex_Writer;
