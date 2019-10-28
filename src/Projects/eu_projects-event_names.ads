package Eu_Projects.Event_Names is
   Begin_Name    : constant Dotted_Identifier := To_Bounded_String ("begin");
   End_Name      : constant Dotted_Identifier := To_Bounded_String ("end");
   Duration_Name : constant Dotted_Identifier := To_Bounded_String ("duration");

   Event_Time_Name : constant Dotted_Identifier := To_Bounded_String ("when");

   Default_Time : constant String := "default";
end Eu_Projects.Event_Names;
