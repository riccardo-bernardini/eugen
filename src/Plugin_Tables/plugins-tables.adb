with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Containers.Vectors;

package body Plugins.Tables is
   use type Ada.Tags.Tag;

   package Plugin_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Plugin_ID,
                                                 Element_Type => Ada.Tags.Tag);

   Plugin_Map : Plugin_Maps.Map;

   procedure Find_Plugin
     (ID      : in     Plugin_ID;
      Success :    out Boolean);

   --------------
   -- Register --
   --------------

   procedure Register
     (ID  : Plugin_ID;
      Tag : Ada.Tags.Tag)
   is
      OK  : Boolean;
      Pos : Plugin_Maps.Cursor;
   begin
      Plugin_Map.Insert (Key      => ID,
                         New_Item => Tag,
                         Position => Pos,
                         Inserted => OK);
   end Register;

   ---------
   -- Get --
   ---------

   function Get (ID                : Plugin_ID;
                 Params            : not null access Plugin_Parameters;
                 Search_If_Missing : Boolean := False)
                 return Root_Plugin_Type'Class is
      OK  : Boolean;
      Tag : Ada.Tags.Tag;

      function New_Plugin is
        new Ada.Tags.Generic_Dispatching_Constructor
          (T           => Root_Plugin_Type,
           Parameters  => Plugin_Parameters,
           Constructor => Constructor);
   begin
      if not Plugin_Map.Contains (ID) then
         if Search_If_Missing then
            Find_Plugin (ID      => ID,
                         Success => OK);
         else
            OK := False;
         end if;

         if not OK then
            raise Unknown_Plugin_ID;
         end if;

         if not Plugin_Map.Contains (ID) then
            raise Program_Error;
         end if;
      end if;

      Tag := Plugin_Map.Element (ID);

      return New_Plugin (Tag, Params);
   end Get;

   function Exists (ID                : Plugin_ID;
                    Search_If_Missing : Boolean := False)
                    return Boolean
   is
      OK : Boolean;
   begin
      if Plugin_Map.Contains (ID) then
         return True;
      elsif Search_If_Missing then
         Find_Plugin (ID      => ID,
                      Success => OK);

         return OK;
      else
         return False;
      end if;
   end Exists;

   procedure For_All_Plugins
     (Callback : not null access procedure (ID : Plugin_ID))
   is
   begin
      for I in Plugin_Map.Iterate loop
         Callback (Plugin_Maps.Key (I));
      end loop;
   end For_All_Plugins;

   subtype Valid_Finder_ID is Finder_ID range 1 .. Finder_ID'Last;

   package Finder_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Valid_Finder_ID,
                                 Element_Type => Finder_Access);

   protected Finder_Table is
      procedure Add (Finder : in     Finder_Access;
                     ID     :    out Finder_ID);

      procedure Remove (ID : Finder_ID);

      procedure Find (ID      : in     Plugin_ID;
                      Success :    out Boolean);
   private
      Finder_Array : Finder_Vectors.Vector;
   end Finder_Table;

   protected body Finder_Table is
      procedure Add (Finder : in     Finder_Access;
                     ID     :    out Finder_ID)
      is
      begin
         Finder_Array.Append (Finder);
         ID := Finder_Array.Last_Index;
      end Add;

      procedure Remove (ID : Finder_ID) is
      begin
         if ID > Finder_Array.Last_Index then
            raise Constraint_Error;
         else
            declare
               procedure Delete (Item : in out Finder_Access) is
               begin
                  Item := null;
               end Delete;
            begin
               Finder_Array.Update_Element (ID, Delete'Access);
            end;
         end if;
      end Remove;

      procedure Find (ID      : in     Plugin_ID;
                      Success :    out Boolean)
      is
         use Finder_Vectors;

         Found : Boolean;
      begin
         for Idx in Finder_Array.Iterate loop
            if Finder_Array.Element (To_Index (Idx)) /= null then
               Finder_Array.Element (To_Index (Idx)).Search_Plugin (ID, Found);

               if Found then
                  Success := True;
                  return;
               end if;
            end if;
         end loop;

         Success := False;
      end Find;
   end Finder_Table;


   function Add_Finder (Finder : Finder_Access) return Finder_ID is
      ID : Finder_ID;
   begin
      Finder_Table.Add (Finder, ID);
      return ID;
   end Add_Finder;

   procedure Remove_Finder (ID : Finder_ID) is
   begin
      Finder_Table.Remove (ID);
   end Remove_Finder;

   ----------
   -- Find --
   ----------

   procedure Find_Plugin
     (ID      : in     Plugin_ID;
      Success :    out Boolean)
   is
   begin
      Finder_Table.Find (ID, Success);
   end Find_Plugin;

end Plugins.Tables;
