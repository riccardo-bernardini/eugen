with SAX.Readers;
with DOM.Readers;
with Input_Sources.Strings;
with Unicode.CES.Utf8;
with Dom.Core.Elements;
with DOM.Core.Nodes;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

package body XML_Utilities is

   procedure Remove_White_Space (Doc : Dom.Core.Node)
   is
      use Dom.Core;
      use Dom.Core.Nodes;

      Children : constant Dom.Core.Node_List := Dom.Core.Nodes.Child_Nodes (Doc);
      N        : Dom.Core.Node;
   begin
      for I in 1 .. Dom.Core.Nodes.Length (Children) loop
         N := Dom.Core.Nodes.Item (Children, I);
         if Node_Type (N) /= Text_Node then
            Remove_White_Space (N);
         else
            declare
               X : constant String := Node_Value (N);
               Q : Node;
            begin
               if (for all I in X'Range => X (I) = ' ' or X (I) = Character'Val (10)) then
                  Q := Remove_Child (Doc, N);
                  Free (Q);
               end if;
            end;
         end if;
      end loop;
   end Remove_White_Space;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String (Item : String) return DOM.Core.Document is
      use SAX.Readers;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      In_Source : Input_Sources.Strings.String_Input;
      Reader    : DOM.Readers.Tree_Reader;
      Result    : Dom.Core.Document;

      Eol_To_Space : constant Character_Mapping := To_Mapping (From => "" & Character'Val (10),
                                                               To   => " ");
   begin
      Input_Sources.Strings.Open (Str      => Translate (Item, Eol_To_Space),
                                  Encoding => Unicode.CES.Utf8.Utf8_Encoding,
                                  Input    => In_Source);

      SAX.Readers.Set_Feature (Parser => Sax_Reader (Reader),
                               Name   => SAX.Readers.Validation_Feature,
                               Value  => False);

      SAX.Readers.Set_Feature (Parser => Sax_Reader (Reader),
                               Name   => SAX.Readers.Namespace_Feature,
                               Value  => False);

      Sax.Readers.Parse (Parser => Sax_Reader (Reader),
                         Input  => In_Source);

      Result := DOM.Readers.Get_Tree (Reader);

      Remove_White_Space (Result);
      return Result;
   end Parse_String;

   function Expect_Attribute (N    : DOM.Core.Node;
                              Name : String)
                              return String
   is
      use type DOM.Core.Node;

      Attr : constant DOM.Core.Node := DOM.Core.Elements.Get_Attribute_Node (N, Name);
   begin
      if Attr = null then
         raise No_Such_Attribute;
      else
         return DOM.Core.Nodes.Node_Value (Attr);
      end if;
   end Expect_Attribute;

   function Get_Attribute (N    : DOM.Core.Node;
                           Name : String;
                           Default : String := "")
                           return String
   is
      use type DOM.Core.Node;

      Attr : constant DOM.Core.Node := DOM.Core.Elements.Get_Attribute_Node (N, Name);
   begin
      if Attr = null then
         return Default;
      else
         return DOM.Core.Nodes.Node_Value (Attr);
      end if;
   end Get_Attribute;

   function Has_Attribute (N    : DOM.Core.Node;
                           Name : String)
                           return Boolean
   is
      use DOM.Core;
   begin
      return Elements.Get_Attribute_Node (N, Name) /= null;
   end Has_Attribute;


end XML_Utilities;
