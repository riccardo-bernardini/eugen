----------------------------------------------------------------------------
--            Generic Command Line Parser (gclp)
--
--               Copyright (C) 2012, Riccardo Bernardini
--
--      This file is part of gclp.
--
--      gclp is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 2 of the License, or
--      (at your option) any later version.
--
--      gclp is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with gclp.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------
--
-- <summary>
-- <p>This is a package implementing a simple-to-use command line
-- parser.  Yes, I know, everyone makes his/her own command line parser...
-- so, I wrote mine.  As they say, every open source project starts
-- with a programmer that schratches its own itch. So I did... If
-- you find this useful, you are welcome to use it.</p>
--
-- <p>The ideas behind this package are the following
--
-- <itemize>
-- <item> <p>Parameters are nominal, non positional.  The syntax is of
--   "named parameter" type, that is, each command line parameter is
--   expected to have thefollowing format</p>
--
--      <center>label ['=' value]</center>
--
--    <p>where "label" is any string without '='.</p></item>
--
-- <item><p> Parsed value are processed by "parameter handlers." A
-- parameter handler is a descendant of Abstract_Parameter_Handler. The
-- operations required to a parameter handler are
--
--    [Receive] Used to give name, value and position of the parameter
--       to the handler that can do whatever it wants with them.  Usually
--       it will convert the value to an internal format and store the
--       result.
--
--     [Is_Set] Return true if Receive has been called at least once. I could
--        not find a way to implement this in the root class, so it is the
--        duty of the concrete handler to define this. I do not like this
--        very much, but it is the "least bad" solution.
--
--     [Reusable] Return true if Receive can be called more than once.
--         Usually only one instance of a parameter is ammitted on a
--         command line, but every now and then we can meet exceptions.
--
-- </p></item>
-- </itemize>
-- </p>
-- The names of the accepted parameters are given to the parser by means
-- of the Add_Parameter procedure by specifying
--
--     + The parameter name
--
--     + A default value (if needed)
--
--     + What to do if the parameter is missing
--
--     + The handler  to be used when the parameter is found
--
-- In order to parse the command line it suffices to call Parse_Command_Line
-- giving as argument the Line_Parser initialized with the  parameter
-- description as said above.  For every parameter found, the Receive
-- procedure of the  corresponding handler  is called.  If at the end
-- of the parsing there are some optional parameters that were missing
-- from the command line, the corresponding handlers are called with
-- the default parameter.
-- </summary>

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;


package Line_Parsers is
   use Ada.Strings.Unbounded;

   type Abstract_Parameter_Handler is interface;

   type Handler_Access is access all Abstract_Parameter_Handler'Class;


   -- Return True if the handler already received at least a value
   function Is_Set (Handler : Abstract_Parameter_Handler) return Boolean
                    is abstract;

   -- Return True if the handler can handle more than one value.
   -- Often it does not make sense to give twice the same parameter,
   -- but in few cases it can be useful
   function Reusable (Handler : Abstract_Parameter_Handler) return Boolean
                      is abstract;

   -- Called to give to the handler a parameter read from command line
   -- Name is the name of the parameter (the part before '='), Value
   -- is the part after '=' and Position is the position of the
   -- parameter on the command line, with the same convention of
   -- Command_Line.Argument
   procedure Receive (Handler  : in out Abstract_Parameter_Handler;
                      Name     : String;
                      Value    : String;
                      Position : Natural)
   is abstract  with
     Pre'Class => not Handler.Is_Set or Handler.Reusable,
     Post'Class => Handler.Is_Set;

   No_Position : constant Natural := 0;
   type Line_Parser (<>) is private;

   -- Use case_sensitive = False if you want case insensitive option matching.
   -- For example, if you set this to False, "input", "Input", "INPUT"
   -- and "InPuT" will be equivalent names for the option "input"
   function Create
     (Case_Sensitive : Boolean := True;
      Normalize_Name : Boolean := True;
      Help_Line      : String := "")
      return Line_Parser;

   type Missing_Action is (Die, Use_Default, Ignore);
   --  Possibile alternatives about what to do if a parameter is missing
   --
   --     [Die]         The parameter is mandatory.  If it is missing, an
   --                   exception with explicative message is raised
   --
   --     [Use_Default] The parameter is optional.  If it is missing, the
   --                   corresponding callback function is called with the
   --                   specified default value (see record
   --                   Parameter_Descriptor in the following)
   --
   --     [Ignore]      The parameter is optional.  If it is missing, nothing
   --                   is done

   -- <description>Record holding the description of a parameter.  The fields
   --  should be self-explenatory (I hope).  The only field that needs some
   -- explanation is Name since it allows to specify more than one
   -- name for each parameter.  The syntax is very simple: just separate
   -- the names with commas.  For example, if Name is "f,filename,input"
   -- one can use on the command line, with the same effect  f=/tmp/a.txt or
   -- filename=/tmp/a.txt or input=/tmp/a.txt.  Spaces at both ends of
   -- the label name are trimmed, so that, for example, "f,filename,input"
   -- is equivalent to "f ,    filename  ,input "
   -- </description>

   procedure Add_Parameter
     (Parser     : in out Line_Parser;
      Name       : String;
      If_Missing : Missing_Action := Ignore;
      Default    : String;
      Handler    : Handler_Access);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => String);

   procedure Parse_Command_Line
     (Parser         : Line_Parser;
      Extend_By      : String_Vectors.Vector := String_Vectors.Empty_Vector;
      Help_Output    : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error);

   -- Main exported method.  It parses the command line and it writes
   -- the result in Result.  If some error is encountered, Bad_Command
   -- is raised with an explicative exception message.  If Help_Line is
   -- not empty, it is written to Help_Output in case of error.

   function Slurp (Filename       : String;
                   Skip_Comments  : Boolean := True;
                   Comment_Char   : Character := '#';
                   Comment_Strict : Boolean := False)
                   return String_Vectors.Vector;
   -- Read the specified filename and returns a vector with the file
   -- lines.  If Skip_Comments is True, lines beginning with Comment_Char
   -- are ignored.  If Comment_Strict is True Comment_Char must be at
   -- the beginning of the line, otherwise initial spaces are allowed.

   Bad_Command : exception;


   --     function Normalized_Form (X : String) return String;

private
   type Parameter_Index is range 1 .. Integer'Last;

   type Parameter_Descriptor is
      record
         Name          : Unbounded_String;
         Handler       : Handler_Access;
         If_Missing    : Missing_Action := Ignore;
         Standard_Name : Unbounded_String;
         Default       : Unbounded_String;
      end record;

   package Parameter_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Parameter_Index,
                                 Element_Type => Parameter_Descriptor);

   -- In order to handle parameter aliases (see comments in the specs)
   -- we keep a table that maps parameter names to parameter "index"
   package Name_To_Index_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => Parameter_Index);


   type Line_Parser is
      record
         Case_Sensitive : Boolean;
         Normalize_Name : Boolean;
         Help_Line      : Unbounded_String;

         Parameters     : Parameter_Vectors.Vector;
         Name_Table     : Name_To_Index_Maps.Map;
      end record;

end Line_Parsers;
