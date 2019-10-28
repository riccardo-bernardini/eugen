with Node_List_Parsers;
with Line_Arrays;
procedure Prova_Parser is
   type Class is (Wp, Tsk, Deliv);

   package My_Parser is new Node_List_Parsers (Class);
   use My_Parser;

   Eol : constant Character := Character'Val (10);
   X   : constant String := "" & Eol
           & "" & Eol
           & "" & Eol
           & "  [  Wp ]" & Eol
           & "name : zorro" & Eol
           & "label : pluto " & Eol
           & "Viva la pappa col pomodoro" & Eol
           & "" & Eol
           & "" & Eol
           & "[task]" & Eol
           & "begin:12" & Eol;

   Names  : My_Parser.Name_Maps.Map;
begin
   Names.Insert (Key      => +"task",
                 New_Item => Tsk);

   declare
      Result : constant Node_List := Parse (Line_Arrays.Split (X), Names);
   begin
      Dump (Result);
   end;
end Prova_Parser;
