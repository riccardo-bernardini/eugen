with Plugins.Tables;

package Prova is
   type Pippo is abstract tagged null record;

   function Constructor (Params : not null access Plugins.No_Parameters)
                         return Pippo is abstract;

   package Table_Int is
     new Plugins.Tables (Root_Plugin_Type  => Pippo,
                         Plugin_ID         => Integer,
                         Plugin_Parameters => Plugins.No_Parameters,
                         Constructor       => Constructor);
end Prova;
