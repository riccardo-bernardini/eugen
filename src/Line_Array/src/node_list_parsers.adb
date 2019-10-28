pragma Ada_2012;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Line_Arrays.Classified;
with Line_Arrays.Regexp_Classifiers;

with GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;

with Tokenize.Token_Vectors;

package body Node_List_Parsers is
   use Ada.Characters.Handling;

    function Chop (X : String) return String
   is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Both));

   function Image (X : Integer) return String
   is (Chop (Integer'Image (X)));
   pragma Unreferenced (Image);

   function Force_Case_Maybe (X : String; Case_Sensitive : Boolean) return String
   is (if Case_Sensitive then X else  To_Lower (X));

   type Line_Class is (Header, Attribute, Text, Comment);

   type Classified_Line (Class : Line_Class) is
      record
         case Class is
            when Header =>
               Name : Id;

            when Attribute =>
               Key   : Id;
               Value : Unbounded_String;

            when Text =>
               Content : Unbounded_String;

            when Comment =>
               null;
         end case;
      end record;

   function Image (Item : Classified_Line) return String
   is (Item.Class'Image &
       (case Item.Class is
           when Header    =>
              Bounded_Identifiers.To_String (Item.Name),
           when Attribute =>
              Bounded_Identifiers.To_String (Item.Key) & "," & To_String (Item.Value),
           when Text      =>
              To_String (Item.Content),
           when Comment   =>
              ""));

   function Extract (Item    : String;
                     Matches : Gnat.Regpat.Match_Array;
                     Index   : Natural)
                     return String
   is (Item (Matches (Index).First .. Matches (Index).Last))
     with Pre => Index <= Matches'Last;

   function Extract (Item    : String;
                     Matches : Gnat.Regpat.Match_Array;
                     Index   : Natural)
                     return Id
   is (Bounded_Identifiers.To_Bounded_String (To_Lower (Extract (Item, Matches, Index))))
     with Pre => Index <= Matches'Last;

   function Extract (Item    : String;
                     Matches : Gnat.Regpat.Match_Array;
                     Index   : Natural)
                     return Unbounded_String
   is (To_Unbounded_String (Extract (Item, Matches, Index)))
     with Pre => Index <= Matches'Last;

   function Process_Header (Item    : String;
                            Matches : Gnat.Regpat.Match_Array)
                            return Classified_Line
   is (Classified_Line'(Class   => Header,
                        Name    => Extract (Item, Matches, 1)));

   function Process_Attribute (Item    : String;
                               Matches : Gnat.Regpat.Match_Array)
                               return Classified_Line
   is (Classified_Line'(Class   => Attribute,
                        Key     => Extract (Item, Matches, 1),
                        Value   => Extract (Item, Matches, 2)));


   function Process_Text (Item    : String;
                          Matches : Gnat.Regpat.Match_Array)
                          return Classified_Line
   is (Classified_Line'(Class   => Text,
                        Content => Extract (Item, Matches, 0)));

   function Process_Comment (Item    : String;
                             Matches : Gnat.Regpat.Match_Array)
                             return Classified_Line;


   pragma Warnings (Off, Process_Comment);

   function Process_Comment (Item    : String;
                             Matches : Gnat.Regpat.Match_Array)
                             return Classified_Line
   is (Classified_Line'(Class => Comment));

   function Is_To_Be_Ignored (Line : Classified_Line) return  Boolean
   is (Line.Class = Comment);


   -----------------------
   -- Use_Default_Names --
   -----------------------

   function Use_Default_Names (Names : Name_Maps.Map) return Name_Maps.Map
   is

      use Name_Maps;

      package Reverse_Maps is
        new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Node_Class,
                                                    Element_Type => Id);

      Rev        : Reverse_Maps.Map;
      True_Names : Name_Maps.Map;
   begin
      for Pos in Names.Iterate loop
         if Rev.Contains (Element (Pos)) then
            raise Constraint_Error;
         end if;

         Rev.Include (Key      => Element (Pos),
                      New_Item => Key (Pos));
      end loop;

      for Cl in Node_Class loop
         if Rev.Contains (Cl) then
            True_Names.Include (Key      => Rev (Cl),
                                New_Item => Cl);
         else
            True_Names.Include (Key      => Bounded_Identifiers.To_Bounded_String (To_Lower (Node_Class'Image (Cl))),
                                New_Item => Cl);
         end if;
      end loop;

      --        for Pos in  True_Names.Iterate loop
      --           Put_Line (To_String (Key (Pos)) & "=>" & Element (Pos)'Image);
      --        end loop;

      return True_Names;
   end Use_Default_Names;

   -----------
   -- Parse --
   -----------

   function Parse
     (Input         : Line_Arrays.Line_Array;
      Names         : Name_Maps.Map := Name_Maps.Empty_Map;
      Trimming      : Trimming_Action := Both;
      Line_Trimming : Trimming_Action := Both)
      return Node_List
   is
      package My_Classifier is
        new Line_Arrays.Regexp_Classifiers (Classified_Line => Classified_Line);

      use My_Classifier;

      package Classified is
        new Line_Arrays.Classified (Classified_Line  => Classified_Line,
                                    Classifier_Type  => Classifier_Type);

      use Classified.Classified_Line_Vectors;

      package Node_Vectors is
        new Ada.Containers.Vectors (Index_Type   => Positive,
                                    Element_Type => Node_Type);

      True_Names : constant Name_Maps.Map := Use_Default_Names (Names);
      Result     : Node_Vectors.Vector;

      Id_Regex   : constant String := "[[:alpha:]]+";

      Classifier : constant Classifier_Type :=
                     Create (
                             (
                             P ("^ *\[ *(" & Id_Regex & ") *\] *$", Process_Header'Access),
                             P ("^#", Process_Comment'Access),
                             P ("^(" & Id_Regex & ") *:(.*)$", Process_Attribute'Access),
                             +Process_Text'Access
                            )
                            );

      C_Lines : constant Classified.Classified_Line_Vectors.Vector :=
                  Classified.Classify (Classifier, Input);

      Pos : Classified.Classified_Line_Vectors.Cursor := C_Lines.First;

      ----------------------
      -- Skip_Empty_Lines --
      ----------------------

      procedure Skip_Empty_Lines (C_Lines : Vector;
                                  Pos     : in out Cursor)
      is
         function Is_Empty (X : String) return Boolean
         is (X'Length = 0 or else (for all Ch of X => Ch = ' '));

      begin
         while Pos /= No_Element
           and then C_Lines (Pos).Class = Text
           and then Is_Empty (To_String (C_Lines (Pos).Content))
         loop
            Next (Pos);
         end loop;

         --           Put_Line ("Fatto!");
      end Skip_Empty_Lines;

      ------------------
      -- Parse_Header --
      ------------------

      procedure Parse_Header (C_Lines   : Vector;
                              Pos       : in out Cursor;
                              This_Node : out Node_Type)
      is
      begin
         if C_Lines (Pos).Class /= Header then
            raise Constraint_Error
              with "expected header found "
              & Image (C_Lines (Pos));
         end if;

         if not True_Names.Contains (C_Lines (Pos).Name) then
            raise Constraint_Error with "unrecognized node '" & Bounded_Identifiers.To_String (C_Lines (Pos).Name) & "'";
         end if;

         This_Node.Class := True_Names.Element (C_Lines (Pos).Name);
         Next (Pos);
      end Parse_Header;

      ----------------------
      -- Parse_Attributes --
      ----------------------

      procedure Parse_Attributes (C_Lines   : Vector;
                                  Pos       : in out Cursor;
                                  This_Node : out Node_Type;
                                  Trimming  : Trimming_Action)
      is
         use Ada.Strings.Fixed;

         function Trim (X : String; How : Trimming_Action) return String
         is (case How is
                when None => X,
                when Head => Trim (X, Ada.Strings.Left),
                when Tail => Trim (X, Ada.Strings.Right),
                when Both => Trim (X, Ada.Strings.Both));
      begin
         while Pos /= No_Element and then C_Lines (Pos).Class = Attribute loop
            This_Node.Attributes.Include (Key      => C_Lines (Pos).Key,
                                          New_Item => Trim (To_String (C_Lines (Pos).Value), Trimming));

            Next (Pos);
         end loop;
      end Parse_Attributes;


      -----------------------
      -- Parse_Description --
      -----------------------

      procedure Parse_Description (C_Lines   : Vector;
                                   Pos       : in out Cursor;
                                   This_Node : out Node_Type)
      is
         function Is_Empty (X : String) return Boolean

         is (X = ""
             or else
               (for all Ch of X => Is_Space (Ch)));

         Buffer : Line_Arrays.Line_Array;
      begin
         while Pos /= No_Element and then C_Lines (Pos).Class = Text loop
            Buffer.Append (To_String (C_Lines (Pos).Content));
            Next (Pos);
         end loop;

         if Line_Trimming = Head or Line_Trimming = Both then
            while not Buffer.Is_Empty and then  Is_Empty (Buffer.First_Element) loop
               Buffer.Delete_First;
            end loop;
         end if;

--           if Buffer.Is_Empty then
--              Put_Line ("VUOTO");
--           else
--              Put_Line ("In testa: (" & Buffer.First_Element & ")");
--           end if;

         if Line_Trimming = Tail or Line_Trimming = Both then
            while not Buffer.Is_Empty and then Is_Empty (Buffer.Last_Element) loop
               Buffer.Delete_Last;
            end loop;
         end if;


         This_Node.Description := Buffer;

      end Parse_Description;

   begin
      Skip_Empty_Lines (C_Lines, Pos);

      while Pos /= No_Element loop
         declare
            This_Node : Node_Type;
         begin
            Parse_Header (C_Lines, Pos, This_Node);

            Parse_Attributes (C_Lines, Pos, This_Node, Trimming);

            Parse_Description (C_Lines, Pos, This_Node);

            Result.Append (This_Node);
         end;
      end loop;

      declare
         R : Node_List (Result.First_Index .. Result.Last_Index);
      begin
         for Idx in R'Range loop
            R (Idx) := Result (Idx);
         end loop;

         return R;
      end;
   end Parse;

   procedure Dump (Item : Node_List)
   is
      use Key_Value_Maps;
   begin
      for Node of Item loop
         Put_Line ("CLASS = " & Node_Class'Image (Node.Class));

         for Pos in Node.Attributes.Iterate loop
            Put_Line ("'" & Bounded_Identifiers.To_String (Key (Pos)) & "' = '" & Element (Pos) & "'");
         end loop;

         Put_Line ("<description>");

         for Line of Node.Description loop
            Put_Line (Line);
         end loop;
         Put_Line ("</description>");
         Put_Line ("******");
      end loop;
   end Dump;

   ----------
   -- Next --
   ----------

   procedure Next (X : in out Node_Scanner)
   is
   begin
      if X.Cursor <= X.Nodes'Last then
         X.Cursor := X.Cursor + 1;
      end if;
   end Next;

   ----------
   -- Prev --
   ----------

   procedure Prev (X : in out Node_Scanner)
   is
   begin
      if X.Cursor > X.Nodes'First then
         X.Cursor := X.Cursor - 1;
      end if;
   end Prev;

   function Enumerative (Spec           : String;
                         Allowed_Values : String;
                         Default        : String := "";
                         Case_Sensitive : Boolean := False)
                         return Attribute_Check
   is
       use Tokenize;

      Allowed : constant Token_Vectors.Vector :=
                  Token_Vectors.To_Vector (Split (To_Be_Splitted    => Allowed_Values,
                                                  Separator         => '|',
                                                  Collate_Separator => False));

      Result : Attribute_Check :=
                 Attribute_Check'(Class          => Enumerative_Class,
                                  Attr           => To_Id (Spec),
                                  Allowed        => ID_Vectors.Empty_Vector,
                                  Default        => To_Unbounded_String (Default),
                                  Case_Sensitive => Case_Sensitive);
   begin
      for A of Allowed loop
         declare
            Tmp : constant String := Force_Case_Maybe (Chop (A), Case_Sensitive);
         begin
            if not Is_Valid_Id (Tmp) then
               raise Constraint_Error with "'" & A & "' is not a valid Id";
            end if;

            Result.Allowed.Append (Bounded_Identifiers.To_Bounded_String (Tmp));
         end;
      end loop;

      return Result;
   end Enumerative;


   -----------------
   -- Alternative --
   -----------------

   function Alternative (Spec : String) return Attribute_Check
   is
      use Tokenize;

      Alternatives : constant Token_Vectors.Vector :=
                       Token_Vectors.To_Vector (Split (To_Be_Splitted    => Spec,
                                                       Separator         => '|',
                                                       Collate_Separator => False));

      Result : Attribute_Check (Alternative_Class);
   begin
      for Alt of Alternatives loop
         if not Is_Valid_Id (Alt) then
            raise Constraint_Error with "'" & Alt & "' is not a valid Id";
         end if;

         Result.Alternatives.Append (Bounded_Identifiers.To_Bounded_String (Alt));
      end loop;

      return Result;
   end Alternative;

   -----------
   -- Check --
   -----------

   procedure Check (Checker : Attribute_Checker;
                    Node    : in out Node_Type)
   is
      function Image (Names : ID_Vectors.Vector) return String
      is
         Result : Unbounded_String;
      begin
         for X of Names loop
            Result := Result & Bounded_Identifiers.To_String (X) & ",";
         end loop;

         return "(" & To_String (Result) & ")";
      end Image;
   begin
      for Ck of Checker.Checks loop
         case Ck.Class is
            when Mandatory_Class =>
               if not Node.Attributes.Contains (Ck.Attribute) then
                  raise Missing_Mandatory with Bounded_Identifiers.To_String (Ck.Attribute);
               end if;

            when Default_Class =>
               if not Node.Attributes.Contains (Ck.Key) then
                  Node.Attributes.Insert (Key      => Ck.Key,
                                          New_Item => To_String (Ck.Value));
               end if;


            when Alternative_Class =>
               declare
                  Counter : Natural := 0;
               begin
                  for Alt of Ck.Alternatives loop
                     if Node.Attributes.Contains (Alt) then
                        Counter := Counter + 1;
                     end if;
                  end loop;

                  if Counter = 0 then
                     raise Missing_Alternative with Image (Ck.Alternatives);

                  elsif Counter > 1 then
                     raise Duplicate_Alternative with Image (Ck.Alternatives);
                  end if;
               end;

            when Enumerative_Class =>
               if not Node.Attributes.Contains (Ck.Attr) then
                  if Ck.Default = Null_Unbounded_String then
                     raise Missing_Mandatory with Bounded_Identifiers.To_String (Ck.Attr);
                  else
                     Node.Attributes.Include (Key      => Ck.attr,
                                              New_Item => To_String (Ck.Default));
                  end if;
               end if;

               declare
                  Given : constant String :=
                            Force_Case_Maybe (Node.Attributes (Ck.Attr), Ck.Case_Sensitive);
               begin
                    Node.Attributes.Include (Key      => Ck.attr,
                                             New_Item => Given);

                  if not Ck.Allowed.Contains (To_Id (Given)) then
                     raise Bad_Enumerative
                       with "Bad enumerative value '" & Given & "' for attribute"
                       & Bounded_Identifiers.To_String (Ck.Attr);
                  end if;
               end;
         end case;
      end loop;
   end Check;

   -------------------------
   -- Generic_Enumerative --
   -------------------------

   function Generic_Enumerative (Spec    : String;
                                 Default : String := "")
                                 return Attribute_Check
   is
      Buffer : Unbounded_String;
   begin
      for Val in Enumerative_Type loop
         Buffer := Buffer & Enumerative_Type'Image (Val);

         if Val < Enumerative_Type'Last then
            Buffer := Buffer & "|";
         end if;
      end loop;

      return Enumerative (Spec           => spec,
                          Allowed_Values => To_String (Buffer),
                          Default        => Default,
                          Case_Sensitive => False);
   end Generic_Enumerative;



end Node_List_Parsers;
