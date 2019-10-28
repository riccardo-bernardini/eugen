with Ada.Strings.Unbounded;                       use Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

package Plugins is
   type Dummy_Type is null record;
   type No_Parameters is access Dummy_Type;
   --  This type is handy for those plugins that require no parameters.

   package Parameter_Maps is new
      Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                              Element_Type => String);

   subtype Parameter_Map is Parameter_Maps.Map;

   type Parameter_Map_Access is access Parameter_Maps.Map;
   -- Another very common case of plugin parameters: a map
   -- parameter name --> parameter value.  The parameters have no special
   -- order and every name has at most one value

   Empty_Map : constant Parameter_Map_Access := new Parameter_Maps.Map'(Parameter_Maps.Empty_Map);

   type Parameter_Pair is
      record
         Name  : Unbounded_String;
         Value : Unbounded_String;
      end record;

   function "<" (L, R : Parameter_Pair) return Boolean
is (L.Name < R.Name or else (L.Name = R.Name and L.Value < R.Value));

   package Parameter_Lists is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Parameter_Pair);

   package Parameter_List_Sorting is
     new Parameter_Lists.Generic_Sorting;


   type Parameter_List is access Parameter_Lists.Vector;
   --  A third common case: a sequence of pairs (name, value).  Note
   --  that in this case there is an ordering and that every name can have
   --  more than one value.  A Parameter_List can be sorted according to
   --  the parameter name by using the subroutines in Parameter_List_Sorting.

end Plugins;
