with Ada.Strings.Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

generic
   type Token_Type is (<>);

   type State_Type is (<>);

   Start_State : State_Type;
   EOF_Token   : Token_Type;
package Finite_State_Scanners is
   type Automata_Table is private;

   procedure Add_Branch (Table : in out Automata_Table;
                         From  : State_Type;
                         On    : Ada.Strings.Maps.Character_Set;
                         To    : State_Type);

   procedure Add_Default (Table : in out Automata_Table;
                          From  : State_Type;
                          To    : State_Type);

   procedure Add_Final  (Table  : in out Automata_Table;
                         From   : State_Type;
                         On     : Ada.Strings.Maps.Character_Set;
                         Result : Token_Type);

   procedure Add_Shortcut  (Table  : in out Automata_Table;
                         From   : State_Type;
                         On     : String;
                         Result : Token_Type);

   procedure Add_Default  (Table  : in out Automata_Table;
                           From   : State_Type;
                           Result : Token_Type);

   type Token_Descriptor (Length : Natural) is
      record
         Token : Token_Type;
         Image : String (1 .. Length);
      end record;

   package Token_Vectors is
         new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                                Element_Type => Token_Descriptor);

   type Token_List is tagged  private;

   function Current (List : Token_List) return Token_Descriptor;

   function Current (List : Token_List) return Token_Type;

   function All_Tokens (List : Token_List) return Token_Vectors.Vector;

   function Scan (Input  : String;
                  Automa : Automata_Table;
                  Skip   : String := " ")
                  return Token_List;
private
   type Transition_Class is (To_Final, To_State, Empty);

   type Transition_Description (Class : Transition_Class := To_State) is
      record
         case Class is
            when To_Final =>
               Result : Token_Type;

            when To_State =>
               Destination : State_Type;

            when Empty =>
               null;
         end case;
      end record;

   Empty_Transition : constant Transition_Description := (Class => Empty);

   type Transition_Map is array (State_Type, Character) of Transition_Description;

   Empty_Transition_Map : constant Transition_Map := (others => (others => Empty_Transition));

   type Shortcut_Descriptor (Length : Natural) is
      record
         Shortcut : String (1 .. Length);
         Result   : Token_Type;
      end record;

   package Shortcut_Vectors is
         new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                                Element_Type => Shortcut_Descriptor);

   use type Shortcut_Vectors.Vector;

   package Shortcut_Maps is
         new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => State_Type,
                                                     Element_Type => Shortcut_Vectors.Vector);
   type Automata_Table is
      record
         Transitions : Transition_Map := Empty_Transition_Map;
         Shortcuts   : Shortcut_Maps.Map := Shortcut_Maps.Empty_Map;
      end record;

   type Token_List is tagged
      record
         Position : Positive;
         L        : Token_Vectors.Vector;
      end record;

   EOF_Descriptor : constant Token_Descriptor := (Length => 0,
                                                  Token  => EOF_Token,
                                                  Image  => "");

   function All_Tokens (List : Token_List) return Token_Vectors.Vector
   is (List.L);

   function Current (List : Token_List) return Token_Descriptor
   is (if List.Position > List.L.Last_Index then
          EOF_Descriptor
       else
          List.L (List.Position));

   function Current (List : Token_List) return Token_Type
   is (List.Current.Token);

end Finite_State_Scanners;
