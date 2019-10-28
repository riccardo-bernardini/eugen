with Ada.Text_IO; use Ada.Text_IO;
package body EU_Projects.Node_Tables is

   procedure Dump (T : Node_Table) is
      use Node_Maps;
   begin
      for Pos in T.Table.Iterate loop
         Put_Line (EU_Projects.Nodes.To_String(Key (Pos)) & ":");
      end loop;
   end Dump;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (T    : in out Node_Table;
      ID   : Nodes.Node_Label;
      Item : Nodes.Node_Access)
   is
   begin
      T.Table.Insert (Key      => ID,
                      New_Item => Item);
   exception
      when Constraint_Error =>
         raise Duplicated_Label;
   end Insert;


   ---------------
   -- Labels_Of --
   ---------------

   function Labels_Of (T     : Node_Table;
                       Class : Nodes.Node_Class)
                       return Nodes.Node_Label_Lists.Vector
   is
      use Node_Maps;
      use type Nodes.Node_Class;

      Result : Nodes.Node_Label_Lists.Vector;
   begin
      for Pos in T.Table.Iterate loop
         if Nodes.Class (Element (Pos).all) = Class then
            Result.Append (Key (Pos));
         end if;
      end loop;

      return Result;
   end Labels_Of;

end EU_Projects.Node_Tables;
