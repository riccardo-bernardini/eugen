--
-- This package provides resources to handle dynamic "plugins"
--
--  A plugin is an "object" that can be called dynamically at run-time to
--  handle, for example, files with different format.  A plugin will
--  typically be a descendant of an abstract type that specifies the
--  minimal interface the plugin must export.  For example, a program
--  that needs to load images that can be in many different formats,
--  it can define an "abstract image reader" with something like
--
--      type Abstract_Image_Reader is interface;
--
--      function  Read_Image (Filename : String) return Image
--      is abstract;
--
--  Successively, one can define plugins to read JPEG, GIF, PNG, ...
--  as descendants of Abstract_Image_Reader.
--
--  Every plugin is identified by a Plugin_ID.  The actual form of
--  the Plugin_ID depends on the implementation, therefore its definition
--  is used as a parameter of this package.  In the case of the image readers,
--  the ID could be a string with the format name.
--
--  Note that while an ID uniquely identifies a plugin, a plugin can
--  be identified by more than an ID.  For example, the plugin to read
--  JPEG images could be identified by both IDs "jpg" and "jpeg" (supposing
--  the IDs case insensitive)

with Ada.Tags;

generic
   type Root_Plugin_Type (<>) is abstract tagged private;
   --  The ancestor of all plugins, the one who specify the minimum
   --  plugin interface.

   type Plugin_Parameters (<>) is limited private;
   --  A type used to pass parameters to the plugin that is going to
   --  be built.  In the "parent" of this package there are few types
   --  that cover the most common cases.

   type Plugin_ID (<>) is private;
   --  The ID used to identify plugins.

   with function "<" (L, R : Plugin_ID) return Boolean is <>;
   --  An ID ordering function

   with function Constructor (Params : not null access Plugin_Parameters)
                              return Root_Plugin_Type is abstract;
   --  A "builder" for the abstract plugin.  The programmer who writes
   --  a new plugin must define this function for the new plugin.

package Plugins.Tables is
   procedure Register (ID  : Plugin_ID;
                       Tag : Ada.Tags.Tag)
     with Pre =>
       Ada.Tags.Is_Descendant_At_Same_Level (Descendant => Tag,
                                             Ancestor   => Root_Plugin_Type'Tag);
   --  Register a new plugin in the table by giving its ID and the Tag of the
   --  corresponding type.  By calling this procedure more than once, one
   --  can register many IDs for the same plugin.

   function Exists (ID                : Plugin_ID;
                    Search_If_Missing : Boolean := False)
                    return Boolean;
   --  Return true if a plugin with the specified ID exists.  If
   --  Search_If_Missing is True, try to search for the plugin with the finder
   --  before returning false.

   function Get (ID                : Plugin_ID;
                 Params            : not null access Plugin_Parameters;
                 Search_If_Missing : Boolean := False)
                 return Root_Plugin_Type'Class;
   --  Get a plugin with the specified ID. Raise Unknown_Plugin_ID if the
   --  ID does not exist. If Search_If_Missing is True, try to search
   --  for the plugin with the finder before raising the exception


   Unknown_Plugin_ID : exception;

   procedure For_All_Plugins
     (Callback : not null access procedure (ID : Plugin_ID));
   --  Call the Callback with every known plugin ID.  Note that in this
   --  case the plugin finder is not used.

   type Abstract_Finder is interface;
   --  A "finder" is an object used to search for plugins (e.g., on disk,
   --  in Internet, ...).  The user can register new finders with the
   --  table.

   procedure Search_Plugin
     (Finder  : in out Abstract_Finder;
      ID      : in     Plugin_ID;
      Success :    out Boolean)
   is abstract;
   --  Search for a plugin with the given ID and set Success = True
   --  if found

   type Finder_Access is access all Abstract_Finder'Class;

   type Finder_ID is private;
   --  Every time a new finder is registered, the package assigns to
   --  it an ID that can be used to remove the finder

   No_Finder : constant Finder_ID;

   function Add_Finder (Finder : Finder_Access) return Finder_ID;
   --  Add a new finder to the table and return an ID that can be
   --  used to remove it

   procedure Remove_Finder (ID : Finder_ID);
   --  Remove the specified finder
private
   type Finder_ID is new Natural;

   No_Finder : constant Finder_ID := Finder_ID'First;
end Plugins.Tables;
