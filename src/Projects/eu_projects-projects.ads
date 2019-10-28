with Ada.Iterator_Interfaces;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;

use Ada;

with EU_Projects.Nodes.Partners;
with EU_Projects.Nodes.Timed_Nodes.Milestones;
with EU_Projects.Nodes.Timed_Nodes.Deliverables;
with EU_Projects.Nodes.Action_Nodes.WPs;
with EU_Projects.Nodes.Action_Nodes.Tasks;
with EU_Projects.Nodes.Risks;
with EU_Projects.Node_Tables;
with EU_Projects.Times;

use EU_Projects.Nodes.Action_Nodes;
use EU_Projects.Nodes.Timed_Nodes;

with EU_Projects.Nodes.Timed_Nodes.Project_Infos;
--  with Ada.Containers.Ordered_Maps;

--------------------------
-- EU_Projects.Projects --
--------------------------

package EU_Projects.Projects is
   type Project_Descriptor is
     new Finalization.Limited_Controlled
   with
     private;

   type Project_Access is access Project_Descriptor;


   function Node_Directory (Item : aliased in out Project_Descriptor)
                            return Node_Tables.Table_Ref;

   function Is_Blessed (Project : Project_Descriptor) return Boolean;

   procedure Bless (Project : in out Project_Descriptor;
                    Info    : Nodes.Timed_Nodes.Project_Infos.Info_Access)
     with
       Pre => not Project.Is_Blessed,
       Post => Project.Is_Blessed;

   procedure Configure (Project : in out Project_Descriptor;
                        Name    : String;
                        Value   : String);

   procedure Add_Partner (Project : in out Project_Descriptor;
                          Partner : in     Nodes.Partners.Partner_Access);

   procedure Add_WP (Project : in out Project_Descriptor;
                     WP      : in     WPs.Project_WP_Access);

   procedure Add_Milestone (Project : in out Project_Descriptor;
                            Item    : in     Nodes.Timed_Nodes.Milestones.Milestone_Access);

   procedure Add_Risk (Project : in out Project_Descriptor;
                       Item    : in     Nodes.Risks.Risk_Access);


   type Freezing_Options is private;

   Sort_Milestones : constant Freezing_Options;

   function "+" (X, Y : Freezing_Options) return Freezing_Options;

   procedure Freeze (Project : in out Project_Descriptor;
                     Options : Freezing_Options := Sort_Milestones)
     with
       Pre => not Project.Is_Freezed and Project.Is_Blessed,
       Post => Project.Is_Freezed;
   --  Do final housekeeping such as checking for duplicate labels, resolving
   --  all the implicit time references in the project and so on.
   --  Raise exception Unsolvable if some references cannot be resolved
   --  (usually because of a circular reference) and Duplicate_Label if
   --  there is more than one node with the same label



   function Is_Freezed (Project : Project_Descriptor) return Boolean;

   Unsolvable       : exception;
   Duplicated_Label : exception;


   function Find (Where : Project_Descriptor;
                  Label : Nodes.Node_Label)
                  return Nodes.Node_Access;
   --  Search for a child with the given name and return it
   --  Return No_Child if the child does not exist

   function Exists (Where : Project_Descriptor;
                    Label : Nodes.Node_Label)
                    return Boolean;

   function Duration (Item : Project_Descriptor) return Times.Instant;

   type Month_Number is new Positive;
   subtype Extended_Month_Number is Month_Number'Base range Month_Number'First - 1 .. Month_Number'Last;

   No_Month : constant Extended_Month_Number := Extended_Month_Number'First;

   type Milestone_Table_Type is
         array (Month_Number range <>, Positive range <>)
         of Nodes.Timed_Nodes.Milestones.Milestone_Access;

   function Milestone_Table (Item : Project_Descriptor) return Milestone_Table_Type
         with Pre => Item.Is_Freezed;

   -- ========= --
   -- ITERATORS --
   -- ========= --

   ------------------
   -- WP Iterators --
   ------------------

   type WP_Cursor is private;

   function Has_Element (X : WP_Cursor) return Boolean;


   package WP_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => WP_Cursor,
                                  Has_Element  => Has_Element);

   function All_WPs
     (Item : Project_Descriptor)
          return WP_Iterator_Interfaces.Forward_Iterator'Class;


   function Element (Index : WP_Cursor) return WPs.Project_WP_Access;

   function N_WPs (Item : Project_Descriptor) return Natural;

   -----------------------
   -- Partner Iterators --
   -----------------------

   type Partner_Cursor is private;

   function Has_Element (X : Partner_Cursor) return Boolean;

   package Partner_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Partner_Cursor,
                                  Has_Element  => Has_Element);
   function All_Partners
     (Item : Project_Descriptor)
          return Partner_Iterator_Interfaces.Forward_Iterator'Class;

   function Element (Index : Partner_Cursor)
                     return Nodes.Partners.Partner_Access;

   function N_Partners (Item : Project_Descriptor) return Natural;


   function Partner_Names (Item : Project_Descriptor)
                           return Nodes.Partners.Partner_Name_Array;
   --------------------
   -- Risk Iterators --
   --------------------

   type Risk_Cursor is private;

   function Has_Element (X : Risk_Cursor) return Boolean;

   package Risk_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Risk_Cursor,
                                  Has_Element  => Has_Element);

   function All_Risks
     (Item : Project_Descriptor)
          return Risk_Iterator_Interfaces.Forward_Iterator'Class;




   -------------------------
   -- All Nodes Iterators --
   -------------------------

   type All_Node_Cursor is private;

   function Has_Element (X : All_Node_Cursor) return Boolean;

   package All_Node_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => All_Node_Cursor,
                                  Has_Element  => Has_Element);

   function All_Nodes
     (Item : Project_Descriptor; Full : Boolean := False)
          return All_Node_Iterator_Interfaces.Forward_Iterator'Class;

   function Element (Index : All_Node_Cursor)
                     return Nodes.Node_Access;


   -------------------------
   -- Milestone Iterators --
   -------------------------

   type Milestone_Cursor is private;

   function Has_Element (X : Milestone_Cursor) return Boolean;

   package Milestone_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => Milestone_Cursor,
                                  Has_Element  => Has_Element);
   function All_Milestones
     (Item : Project_Descriptor)
          return Milestone_Iterator_Interfaces.Forward_Iterator'Class;

   function Element (Index : Milestone_Cursor)
                     return Nodes.Timed_Nodes.Milestones.Milestone_Access;

   -----------------------
   -- All task iteration --
   ------------------------

   type All_Task_Cursor is private;


   function Element (Index : All_Task_Cursor) return Tasks.Project_Task_Access;


   function Has_Element (X : All_Task_Cursor) return Boolean;


   package All_Task_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => All_Task_Cursor,
                                  Has_Element  => Has_Element);


   function All_Tasks (Item : Project_Descriptor)
                       return All_Task_Iterator_Interfaces.Forward_Iterator'Class;


   type All_Deliverable_Cursor is private;


   function Element (Index : All_Deliverable_Cursor) return Deliverables.Deliverable_Access;


   function Has_Element (X : All_Deliverable_Cursor) return Boolean;


   package All_Deliverable_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor       => All_Deliverable_Cursor,
                                  Has_Element  => Has_Element);


   function All_Deliverables (Item : Project_Descriptor)
                              return All_Deliverable_Iterator_Interfaces.Forward_Iterator'Class;



private
   use WPs, Nodes.Partners, Nodes.Timed_Nodes.Milestones;
   use Nodes, Nodes.Risks;

   --     procedure Add_Node (Project : in out Project_Descriptor;
   --                         Label   : Nodes.Node_Label;
   --                         Node    : Nodes.Node_Access);

   type Cursor is null record;





   --     type Deliverable_Cursor is
   --       new Nodes.Node_Index'Base range Nodes.Node_Index'First - 1 .. Nodes.Node_Index'Last;
   --
   --     type Milestone_Cursor is
   --       new Nodes.Node_Index'Base range Nodes.Node_Index'First - 1 .. Nodes.Node_Index'Last;
   --
   --
   package WP_Lists is
     new Ada.Containers.Vectors (Index_Type   => WP_Index,
                                 Element_Type => Project_WP_Access);


   package Partner_Lists is
     new Ada.Containers.Vectors (Index_Type   => Partner_Index,
                                 Element_Type => Partner_Access);

   package Risk_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Risks.Risk_Index,
                                 Element_Type => Risks.Risk_Access);



   package Milestone_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Milestones.Milestone_Index,
                                 Element_Type => Milestones.Milestone_Access);

   use type Times.Instant;

   function Is_Earlier (X, Y : Milestones.Milestone_Access) return Boolean
   is (Times.Instant'(X.Due_On) < Times.Instant'(Y.Due_On));

   package Milestone_Sort is
     new Milestone_Vectors.Generic_Sorting ("<" => Is_Earlier);

   package Option_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => String);

   --     package Node_Maps is
   --       new Ada.Containers.Ordered_Maps (Key_Type     => Nodes.Node_Label,
   --                                        Element_Type => Nodes.Node_Access);

   use type Nodes.Timed_Nodes.Project_Infos.Info_Access;

   type Project_Descriptor is
     new Finalization.Limited_Controlled
   with
      record
         Options        : Option_Maps.Map;
         WP_List        : WP_Lists.Vector;
         Partner_List   : Partner_Lists.Vector;
         Risk_List      : Risk_Vectors.Vector;
         Milestones     : Milestone_Vectors.Vector;
         Node_Directory : aliased Node_Tables.Node_Table;
         Freezed        : Boolean := False;
         Last_Month     : Times.Instant := Times.Earliest_Istant;
         Info           : Project_Infos.Info_Access := null;
      end record;

   function Is_Blessed (Project : Project_Descriptor) return Boolean
   is (Project.Info /= null);

   function Duration (Item : Project_Descriptor) return Times.Instant
   is (Item.Last_Month);

   function N_Partners (Item : Project_Descriptor) return Natural
   is (Natural (Item.Partner_List.Length));

   function N_WPs (Item : Project_Descriptor) return Natural
   is (Natural (Item.WP_List.Length));


   function Node_Directory (Item : aliased in out Project_Descriptor)
                            return Node_Tables.Table_Ref
   is (D => Item.Node_Directory'Access);


   -------------------------
   -- Milestone iteration --
   -------------------------


   type Milestone_Cursor is new Milestone_Vectors.Cursor;

   type Milestone_Iterator is
     new Milestone_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Milestone_Vectors.Cursor;
      end record;

   overriding
   function First (Object : Milestone_Iterator) return Milestone_Cursor
   is (Milestone_Cursor (Object.Start));

   overriding
   function Next
     (Object   : Milestone_Iterator;
      Position : Milestone_Cursor) return Milestone_Cursor
   is (Milestone_Cursor (Milestone_Vectors.Next (Milestone_Vectors.Cursor (Position))));

   function Element (Index : Milestone_Cursor) return Nodes.Timed_Nodes.Milestones.Milestone_Access
   is (Milestone_Vectors.Element (Milestone_Vectors.Cursor (Index)));

   function Has_Element (X : Milestone_Cursor) return Boolean
   is (Milestone_Vectors.Has_Element (Milestone_Vectors.Cursor (X)));


   function All_Milestones (Item : Project_Descriptor)
                            return Milestone_Iterator_Interfaces.Forward_Iterator'Class
   is (Milestone_Iterator'(Start => Item.Milestones.First));

   ------------------
   -- WP iteration --
   ------------------
   type WP_Cursor is new WP_Lists.Cursor;


   type Task_Cursor is
      record
         WP  : WP_Cursor;
         Tsk : Nodes.Node_Index;
      end record;

   type WP_Iterator is
     new WP_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : WP_Cursor;
      end record;

   overriding
   function First (Object : WP_Iterator) return WP_Cursor
   is (Object.Start);

   overriding
   function Next
     (Object   : WP_Iterator;
      Position : WP_Cursor) return WP_Cursor
   is (WP_Cursor (WP_Lists.Next (WP_Lists.Cursor (Position))));


   function Element (Index : WP_Cursor) return Nodes.Action_Nodes.WPs.Project_WP_Access
   is (WP_Lists.Element (WP_Lists.Cursor (Index)));


   function Has_Element (X : WP_Cursor) return Boolean
   is (WP_Lists.Has_Element (WP_Lists.Cursor (X)));

   function All_WPs (Item : Project_Descriptor)
                     return WP_Iterator_Interfaces.Forward_Iterator'Class
   is (WP_Iterator'(Start => WP_Cursor (Item.WP_List.First)));

   -----------------------
   -- Partner iteration --
   -----------------------

   type Partner_Cursor is new Partner_Lists.Cursor;



   type Partner_Iterator is
     new Partner_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Partner_Lists.Cursor;
      end record;

   overriding
   function First (Object : Partner_Iterator) return Partner_Cursor
   is (Partner_Cursor (Object.Start));

   overriding
   function Next
     (Object   : Partner_Iterator;
      Position : Partner_Cursor) return Partner_Cursor
   is (Partner_Cursor (Partner_Lists.Next (Partner_Lists.Cursor (Position))));


   function Element (Index : Partner_Cursor) return Nodes.Partners.Partner_Access
   is (Partner_Lists.Element (Partner_Lists.Cursor (Index)));


   function Has_Element (X : Partner_Cursor) return Boolean
   is (Partner_Lists.Has_Element (Partner_Lists.Cursor (X)));

   function All_Partners (Item : Project_Descriptor)
                          return Partner_Iterator_Interfaces.Forward_Iterator'Class
   is (Partner_Iterator'(Start => Item.Partner_List.First));

   ------------------------
   -- All task iteration --
   ------------------------

   type All_Task_Cursor is
      record
         WP_Pos   : WP_Cursor;
         Task_Pos : WPs.Task_Cursor;

         Task_Iter : WPs.Task_Iterator;
      end record;


   function Element (Index : All_Task_Cursor) return Tasks.Project_Task_Access
   is (WPs.Element (Index.Task_Pos));


   function Has_Element (X : All_Task_Cursor) return Boolean
   is (Has_Element (X.WP_Pos));


   type All_Task_Iterator is
     new All_Task_Iterator_Interfaces.Forward_Iterator
   with
      record
         WP_Iter   : WP_Iterator;
      end record;


   overriding
   function First (Object : All_Task_Iterator) return All_Task_Cursor;

   overriding
   function Next
     (Object   : All_Task_Iterator;
      Position : All_Task_Cursor) return All_Task_Cursor;



   function All_Tasks (Item : Project_Descriptor)
                       return All_Task_Iterator_Interfaces.Forward_Iterator'Class
   is (All_Task_Iterator'(WP_Iter => WP_Iterator (Item.All_WPs)));

   ------------------------------
   -- All_Deliverable_Iterator --
   ------------------------------

   type All_Deliverable_Iterator is
     new All_Deliverable_Iterator_Interfaces.Forward_Iterator
   with
      record
         WP_Iter   : WP_Iterator;
      end record;

   type All_Deliverable_Cursor is
      record
         WP_Pos    : WP_Cursor;
         Deliv_Pos : WPs.Deliverable_Cursor;

         Deliv_Iter : WPs.Deliverable_Iterator;
      end record;


   function Element (Index : All_Deliverable_Cursor) return Deliverables.Deliverable_Access
   is (WPs.Element (Index.Deliv_Pos));


   function Has_Element (X : All_Deliverable_Cursor) return Boolean
   is (Has_Element (X.Deliv_Pos));


   overriding
   function First (Object : All_Deliverable_Iterator) return All_Deliverable_Cursor;

   overriding
   function Next
     (Object   : All_Deliverable_Iterator;
      Position : All_Deliverable_Cursor) return All_Deliverable_Cursor;

   function All_Deliverables (Item : Project_Descriptor)
                       return All_Deliverable_Iterator_Interfaces.Forward_Iterator'Class
   is (All_Deliverable_Iterator'(WP_Iter => WP_Iterator (Item.All_WPs)));
   --------------------
   -- Risk iteration --
   --------------------

   type Risk_Cursor is new Risk_Vectors.Cursor;



   type Risk_Iterator is
     new Risk_Iterator_Interfaces.Forward_Iterator
   with
      record
         Start : Risk_Vectors.Cursor;
      end record;

   overriding
   function First (Object : Risk_Iterator) return Risk_Cursor
   is (Risk_Cursor (Object.Start));

   overriding
   function Next
     (Object   : Risk_Iterator;
      Position : Risk_Cursor) return Risk_Cursor
   is (Risk_Cursor (Risk_Vectors.Next (Risk_Vectors.Cursor (Position))));


   function Element (Index : Risk_Cursor) return Risks.Risk_Access
   is (Risk_Vectors.Element (Risk_Vectors.Cursor (Index)));


   function Has_Element (X : Risk_Cursor) return Boolean
   is (Risk_Vectors.Has_Element (Risk_Vectors.Cursor (X)));

   function All_Risks (Item : Project_Descriptor)
                       return Risk_Iterator_Interfaces.Forward_Iterator'Class
   is (Risk_Iterator'(Start => Item.Risk_List.First));

   -------------------------
   --  All Node iteration --
   -------------------------

   subtype Restricted_Node_Class is
     Nodes.Node_Class
   range
     Node_Class'Succ (Nodes.Info_Node) .. Node_Class'Last;

   type All_Node_Cursor is
      record
         Prj_Info_Access : Nodes.Timed_Nodes.Project_Infos.Info_Access;
         Current_Class   : Restricted_Node_Class;
         Risk_Pos        : Risk_Cursor;
         Deliverable_Pos : All_Deliverable_Cursor;
         WP_Pos          : WP_Cursor;
         Task_Pos        : All_Task_Cursor;
         Partner_Pos     : Partner_Cursor;
         Milestone_Pos   : Milestone_Cursor;
      end record;

   type All_Node_Iterator is
     new All_Node_Iterator_Interfaces.Forward_Iterator
   with
      record
         Prj_Info_Access  : Project_Infos.Info_Access;
         Risk_Iter        : Risk_Iterator;
         Deliverable_Iter : All_Deliverable_Iterator;
         WP_Iter          : WP_Iterator;
         All_Task_Iter    : All_Task_Iterator;
         Partner_Iter     : Partner_Iterator;
         Milestone_Iter   : Milestone_Iterator;
      end record;


   overriding
   function First (Object : All_Node_Iterator) return All_Node_Cursor
   is (All_Node_Cursor'(Current_Class   => Node_Class'Succ (Info_Node),
                        Prj_Info_Access => Object.Prj_Info_Access,
                        Risk_Pos        => First (Object.Risk_Iter),
                        Deliverable_Pos => First (Object.Deliverable_Iter),
                        WP_Pos          => First (Object.WP_Iter),
                        Task_Pos        => First (Object.All_Task_Iter),
                        Partner_Pos     => First (Object.Partner_Iter),
                        Milestone_Pos   => First (Object.Milestone_Iter)));

   overriding
   function Next
     (Object   : All_Node_Iterator;
      Position : All_Node_Cursor) return All_Node_Cursor;

   function Element (Index : All_Node_Cursor) return Node_Access
   is (if Index.Prj_Info_Access /= null
       then
          Node_Access (Index.Prj_Info_Access)
       else
         (case Index.Current_Class is
             when WP_Node          => Node_Access (Element (Index.WP_Pos)),
             when Task_Node        => Node_Access (Element (Index.Task_Pos)),
             when Deliverable_Node => Node_Access (Element (Index.Deliverable_Pos)),
             when Risk_Node        => Node_Access (Element (Index.Risk_Pos)),
             when Partner_Node     => Node_Access (Element (Index.Partner_Pos)),
             when Milestone_Node   => Node_Access (Element (Index.Milestone_Pos))));

   function Has_Element (X : All_Node_Cursor) return Boolean
   is (X.Prj_Info_Access /= null
       or else X.Current_Class /= Nodes.Node_Class'Last
       or else Has_Element (X.Partner_Pos));
   pragma Compile_Time_Error (Node_Class'Last /= Partner_Node, "Wrong has_element");

   function All_Nodes (Item : Project_Descriptor; Full : Boolean := False)
                       return All_Node_Iterator_Interfaces.Forward_Iterator'Class
   is (All_Node_Iterator'
         (Prj_Info_Access  => (if Full then Item.Info else null),
          Risk_Iter        => Risk_Iterator (Item.All_Risks),
          Deliverable_Iter => All_Deliverable_Iterator (Item.All_Deliverables),
          WP_Iter          => WP_Iterator (Item.All_WPs),
          All_Task_Iter    => All_Task_Iterator (Item.All_Tasks),
          Partner_Iter     => Partner_Iterator (Item.All_Partners),
          Milestone_Iter   => Milestone_Iterator (Item.All_Milestones)));


   function Is_Freezed (Project : Project_Descriptor) return Boolean
   is (Project.Freezed);

   type Freezing_Basic_Option is (Milestone_Sorting);

   type Freezing_Options is array (Freezing_Basic_Option) of Boolean;

   pragma Warnings(Off, "there are no others");
   Sort_Milestones : constant Freezing_Options := (Milestone_Sorting => True, others => False);

   function "+" (X, Y : Freezing_Options) return Freezing_Options
   is (X or Y);

end EU_Projects.Projects;

--     function Find (Resolver : Project_Descriptor;
--                    What     : Identifiers.Full_Paths.Full_ID)
--                    return Boolean;

--     function Get_Attribute (Project : Project_Descriptor;
--                             Name    : Identifier)
--                             return String;
--
--     function Get_Attribute (Project : Project_Descriptor;
--                             Name    : Identifier)
--                             return Integer;

--     function Get (Project : Project_Descriptor;
--                   Name    : WPs.WP_Label)
--                   return WPs.Project_WP_Access;
--
--     function Get (Project : Project_Descriptor;
--                   Name    : Nodes.Partners.Partner_Label)
--                   return Nodes.Partners.Partner_Access;
