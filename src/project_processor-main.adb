with Project_Processor.Configuration;
with Project_Processor.Parsers.Abstract_Parsers;
with Project_Processor.Parsers.Parser_Tables;
--  with Project_Processor.Parsers.XML_Parsers;
with EU_Projects.Projects;
with Project_Processor.Processors.Processor_Tables;
use Project_Processor.Processors;

use Project_Processor.Parsers;
use EU_Projects.Projects;
use Project_Processor.Processors;
with Ada.Text_IO; use Ada.Text_IO;

with EU_Projects.Times.Time_Expressions.Solving;

with Ada.Exceptions;

use Ada;

pragma Warnings (Off);

with Project_Processor.Parsers.Simple_Format;

with Project_Processor.Processors.Dumping;
with Project_Processor.Processors.LaTeX;

pragma Warnings (On);
procedure Project_Processor.Main is
   Project : Project_Descriptor;
   Current_Processor : Processors.Processor_ID;

   procedure Do_Call (Call : Configuration.Processing_Call) is
      Unknown_Processor : exception;

   begin
      if not Processor_Tables.Exists (Call.Name) then
         raise Unknown_Processor
           with Project_Processor.Processors.Image (Call.Name);
      end if;

      Current_Processor := Call.Name;

      declare
         Proc : Abstract_Processor'Class :=
                  Processor_Tables.Get (ID                => Call.Name,
                                        Params            => Call.Parameters,
                                        Search_If_Missing => False);
      begin
         Proc.Process (Project);
      end;
   exception
      when Unknown_Processor =>
         Put_Line (File => Standard_Error,
                   Item => "WARNING: Processor '"
                   & To_String (Call.Name)
                   & "' unknown.  Skipped");

   end Do_Call;

begin
   Configuration.Initialize;

   declare
      Parser : Abstract_Parsers.Abstract_Parser'Class :=
                 Parsers.Parser_Tables.Get
                   (ID                => Configuration.Input_Format,
                    Params            => Configuration.Parser_Parameters,
                    Search_If_Missing => False);


   begin
      Parser.Parse (Project => Project,
                    Input   => Configuration.Input_Data);

      Project.Freeze;

      Configuration.For_All_Calls (Callback => Do_Call'Access);
   end;
exception
   when EU_Projects.Times.Time_Expressions.Solving.Unsolvable =>
      Put_Line (Standard_Error, "Could not solve");

   when E : Configuration.Bad_Command_Line =>
      Put_Line (Standard_Error,
                "Bad Command Line: " & Exceptions.Exception_Message (E));

   when E : EU_Projects.Bad_Input | Parsing_Error =>
      Put_Line (Standard_Error,
                "Bad Project Specs : " & Exceptions.Exception_Message (E));

   when E : Project_Processor.Processors.Processor_Error =>
      Put_Line (Standard_Error,
                "Error in "
                & Processors.Image (Current_Processor)
                & " processor : "
                & Exceptions.Exception_Message (E));


end Project_Processor.Main;
