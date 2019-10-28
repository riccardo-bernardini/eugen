pragma Ada_2012;
with Ada.Strings.Fixed;
package body Latex_Writer is
   -- The result of function 'Image associated to discrete types has
   -- a space at the beginning.  That space is quite annoying and needs
   -- to be trimmed.  This function is here so that everyone can use it

   function Chop (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Image (X : Integer) return String
   is (Chop (Integer'Image (X)));

   -------------------------
   -- Print_Default_Macro --
   -------------------------

   procedure Print_Default_Macro (Output        : File_Access;
                                  Command_Name  : String;
                                  Definition    : String;
                                  N_Parameters  : Natural)
   is
   begin
      Put (File =>
                 Output.all,
           Item =>
                 "\ifdef{" & Command_Name & "}"
           & "{}"
           & "{\newcommand{" & Command_Name & "}");

      if N_Parameters > 0 then
         Put (Output.all, "[" & Image (N_Parameters) & "]");
      end if;

      Put_Line (Output.all, "{" & Definition & "}}");
   end Print_Default_Macro;

   ------------
   -- Within --
   ------------

   procedure Within
         (Output    : File_Access;
          Env_Name  : String;
          Callback  : access procedure (Output : File_Access);
          Parameter : String := "")
   is
   begin
      Put (Output.all, "\begin{" & Env_Name & "}");

      if Parameter /= "" then
         Put (Output.all, "{" & Parameter & "}");
      end if;

      New_Line (Output.all);

      Callback (Output);

      New_Line (Output.all);
      Put_Line (Output.all, "\end{" & Env_Name & "}");
   end Within;



   -----------------------
   -- Within_Table_Like --
   -----------------------

   procedure Within_Table_Like
         (Output    : File_Access;
          Env_Name  : String;
          Callback  : access procedure (Output : File_Access;
                                        Table : in out Table_Handler))
   is
      Arg : constant Parameter_List (2 .. 1) := (others => <>);
   begin
      Within_Table_Like (Output     => Output,
                         Env_Name   => Env_Name,
                         Callback   => Callback,
                         Parameters => Arg);
   end Within_Table_Like;

   -----------------------
   -- Within_Table_Like --
   -----------------------

   procedure Within_Table_Like
         (Output     : File_Access;
          Env_Name   : String;
          Callback   : access procedure (Output : File_Access;
                                         Table : in out Table_Handler);
          Parameter  : String)
   is
   begin
      Within_Table_Like (Output     => Output,
                         Env_Name   => Env_Name,
                         Callback   => Callback,
                         Parameters => (1 => To_Unbounded_String (Parameter)));
   end Within_Table_Like;

   -----------------------
   -- Within_Table_Like --
   -----------------------

   procedure Within_Table_Like
         (Output     : File_Access;
          Env_Name   : String;
          Callback   : access procedure (Output : File_Access;
                                         Table : in out Table_Handler);
          Parameters : Parameter_List)
   is
      T : Table_Handler := Table_Handler'(State         => Begin_Row,
                                          Output        => Output,
                                          Default_Style => To_Unbounded_String (""),
                                          Default_Head  => To_Unbounded_String (""));

   begin
      Put (Output.all, "\begin{" & Env_Name & "}");

      for K in Parameters'Range loop
         Put (Output.all, "{" & To_String (Parameters (K)) & "}");
      end loop;

      New_Line (Output.all);

      Callback (Output, T);


      New_Line (Output.all);
      Put_Line (Output.all, "\end{" & Env_Name & "}");
   end Within_Table_Like;

   ------------------
   -- Within_Table --
   ------------------

   procedure Within_Table
         (Output        : File_Access;
          Table_Spec    : String;
          Callback      : access procedure (Output : File_Access;
                                            Table : in out Table_Handler);
          Default_Style : String := "";
          Default_Head  : String := "";
          Caption       : String := "";
          Width         : String := "\textwidth")
   is
      use Ada.Strings.Fixed;

      T : Table_Handler := Table_Handler'(State         => Begin_Row,
                                          Output        => Output,
                                          Default_Style => To_Unbounded_String (Default_Style),
                                          Default_Head  => To_Unbounded_String (Default_Head));

      Use_Tabularx : constant Boolean := Index (Table_Spec, "X") > 0;

      Env_Name : constant String :=
                   (if Use_Tabularx then "tabularx" else "tabular");

      Width_Spec : constant String := (if Use_Tabularx then "{" & Width & "}" else "");
   begin
      if Caption /= "" then
         Put_Line (Output.all, "\begin{table}");
         Put_Line (Output.all, "\caption{" & Caption & "}");
      end if;

      Put_Line (Output.all, "\centering");

      Put_Line (Output.all,
                "\begin{" & Env_Name & "}"
                & Width_Spec
                & "{" & Table_Spec & "}");

      Callback (Output, T);

      New_Line (Output.all);
      Put_Line (Output.all, "\end{" & Env_Name & "}");

      if Caption /= "" then
         Put_Line (Output.all, "\end{table}");
      end if;
   end Within_Table;

   -----------------
   -- Apply_Style --
   -----------------

   function Apply_Style (Content       : String;
                         Style         : Style_Spec;
                         Default_Style : Unbounded_String)
                         return String
   is
   begin
      if Style /= "" then
         return String (Style) & "{" & Content & "}";

      elsif Default_Style /= "" then
         return To_String (Default_Style) & "{" & Content & "}";

      else
         return Content;
      end if;
   end Apply_Style;

   procedure Put_If_In_State (Table   : in out Table_Handler;
                              Content : String;
                              State   : Table_State)
   is
   begin
      if Table.State = State then
         Put (Table.Output.all, Content);
      end if;
   end Put_If_In_State;
   ---------
   -- Put --
   ---------

   procedure Put
         (Table : in out Table_Handler; Content : String; Style : String := "")
   is
   begin

      Put_If_In_State (Table, " & ", Middle_Row);

      Put (Table.Output.all, Apply_Style (Content, Style_Spec (Style), Table.Default_Style));

      Table.State := Middle_Row;
   end Put;

   -------------
   -- New_Row --
   -------------

   procedure New_Row (Table : in out Table_Handler) is
   begin
      Put_Line (Table.Output.all, "\\");
      Table.State := Begin_Row;
   end New_Row;

   -----------
   -- hline --
   -----------

   procedure Hline (Table : in out Table_Handler;
                    Full  : Boolean := True) is
   begin
      Put_If_In_State (Table, "\\", Middle_Row);

      if Full then
         Put_Line (Table.Output.all, "\hline");
      end if;

      Table.State := Begin_Row;
   end Hline;

   procedure Cline (Table : in out Table_Handler; From, To : Positive)
   is
   begin
      Put_If_In_State (Table, "\\", Middle_Row);

      Put_Line (Table.Output.all, "\cline{" & Image (From) & "-" & Image (To) & "}");

      Table.State := Begin_Row;
   end Cline;


   -----------------
   -- Multicolumn --
   -----------------

   procedure Multicolumn (Table   : in out Table_Handler;
                          Span    : Positive;
                          Spec    : String;
                          Content : String)
   is
   begin
      Put_If_In_State (Table, "&", Middle_Row);

      Put (Table.Output.all, "\multicolumn{" & Image (Span) & "}{" & Spec & "}");
      Put_Line (Table.Output.all, "{" & Content & "}");

      Table.State := Middle_Row;
   end Multicolumn;

   ----------
   -- Head --
   ----------

   procedure Head (Table   : in out Table_Handler;
                   Content : String;
                   Bar     : Bar_Position := Default;
                   Style   : String := "")
   is
      True_Bar : constant Bar_Position :=
                   (if Bar /= Default
                    then
                       Bar
                    elsif Table.State = Begin_Row then
                       Both
                    else
                       Right);

      function Bar_Maybe (X : Boolean)  return String
      is (if X then "|" else "");
   begin
      Table.Multicolumn (Span    => 1,
                         Content => Apply_Style (Content       => Content,
                                                 Style         => Style_Spec (Style),
                                                 Default_Style => Table.Default_Head),
                         Spec    =>
                               Bar_Maybe (True_Bar = Left or True_Bar = Both) &
                               "c" &
                               Bar_Maybe (True_Bar = Right or True_Bar = Both));
   end Head;

   ----------
   -- Hbox --
   ----------

   function Hbox (Content : String;
                  Size    : Latex_Length := Zero;
                  Align   : Align_Type := Center)
                  return String
   is
   begin
      return "\hbox to "
            & Image (Size)
            & "{"
            & (case Align is
                  when Center | Left => "\hss",
                  when Right         => "")
         & "{" & Content & "}"
            & (case Align is
                  when Center | Right => "\hss",
                  when Left           => "")
         & "}";
   end Hbox;

end Latex_Writer;
