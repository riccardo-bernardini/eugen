with Ada.Finalization; use Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;
package body EU_Projects.Nodes.Timed_Nodes.Deliverables is

   ------------
   -- Create --
   ------------

   function Create
     (Label             : Deliverable_Label;
      Name              : String;
      Description       : String;
      Short_Name        : String;
      Delivered_By      : Node_Label_Lists.Vector;
      Due_On            : String;
      Node_Dir          : in out Node_Tables.Node_Table;
      Parent_WP         : Node_Access;
      Linked_Milestones : Nodes.Node_Label_Lists.Vector;
      Deliv_Type        : Deliverable_Type;
      Dissemination     : Dissemination_Level)
      return Deliverable_Access
   is
      Result : Deliverable_Access;
   begin
      Result := new Deliverable'(Controlled with
                                   Label             => Node_Label (Label),
                                 Name              => To_Unbounded_String (Name),
                                 Class             => Deliverable_Node,
                                 Short_Name        => To_Unbounded_String (Short_Name),
                                 Index             => No_Deliverable,
                                 Description       => To_Unbounded_String (Description),
                                 Attributes        => Attribute_Maps.Empty_Map,
                                 Expected_Raw      => To_Unbounded_String (Due_On),
                                 Expected_Symbolic => <>,
                                 Expected_On       => <>,
                                 Fixed             => False,
                                 Deliverer         => Delivered_By,
                                 Parent_WP         => Parent_WP,
                                 Linked_Milestones => Linked_Milestones,
                                 Deliv_Type        => Deliv_Type,
                                 Dissemination     => Dissemination,
                                 Status            => Stand_Alone,
                                 Clone_List        => Deliverable_Arrays.Empty_Vector,
                                 Clone_Index       => 0);

      Node_Dir.Insert (ID   => Node_Label (Label),
                       Item => Node_Access (Result));

      return Result;
   end Create;

   procedure Clone (Item      : in out Deliverable;
                    Due_On    : in     String;
                    Node_Dir  : in out Node_Tables.Node_Table)

   is
      procedure Make_Clone (Clone_Due_On : Unbounded_String)
      is
         use type Ada.Containers.Count_Type;

         function New_Clone_Label (Item : Deliverable) return Node_Label
         is (To_ID (To_String (Item.Label) & "_" & Image (Natural (Item.Clone_List.Length + 1))));

         Result : constant Deliverable_Access :=
                    new Deliverable'(Controlled with
                                     Class             => Item.Class,
                                     Label             => New_Clone_Label (Item),
                                     Name              => Item.Name,
                                     Short_Name        => Item.Short_Name,
                                     Index             => Item.Index,
                                     Description       => Item.Description,
                                     Attributes        => Item.Attributes,
                                     Expected_Raw      => Clone_Due_On,
                                     Expected_Symbolic => Item.Expected_Symbolic,
                                     Expected_On       => Item.Expected_On,
                                     Fixed             => Item.Fixed,
                                     Status            => Clone,
                                     Clone_Index       => Natural (Item.Clone_List.Length) + 1,
                                     Deliverer         => Item.Deliverer,
                                     Parent_WP         => Item.Parent_Wp,
                                     Linked_Milestones => Item.Linked_Milestones,
                                     Deliv_Type        => Item.Deliv_Type,
                                     Dissemination     => Item.Dissemination,
                                     Clone_List        => Deliverable_Arrays.Empty_Vector);
      begin
         Item.Clone_List.Append (Result);

         Node_Dir.Insert (ID   => Result.Label,
                          Item => Node_Access (Result));

         Item.Status := Parent;
      end Make_Clone;
   begin
      if Item.Status = Stand_Alone then
         --
         -- If Item is currently a stand-alone deliverable, we change
         -- it into a Parent, but first we need to create a new clone
         --

         Make_Clone (Item.Expected_Raw);
      end if;

      --
      -- Here for sure Item must be a Parent
      --
      pragma Assert (Item.Status = Parent);

      Make_Clone (To_Unbounded_String (Due_On));

   end Clone;


   overriding function Due_On (Item : Deliverable) return Times.Instant
   is (Item.Expected_On);

   ------------
   -- Due_On --
   ------------

   function Due_On (Item   : Deliverable;
                    Sorted : Boolean := True) return Instant_Vectors.Vector
   is
      package Sorting is
        new Instant_Vectors.Generic_Sorting;

      Result : Instant_Vectors.Vector;
   begin
      case Item.Status is
         when Clone | Stand_Alone =>
            Result.Append (Item.Expected_On);

         when Parent =>
            for Child of Item.Clone_List loop

               Result.Append (Child.Expected_On);
            end loop;
      end case;

      if Sorted then
         Sorting.Sort (Result);
      end if;

      return Result;
   end Due_On;

   -----------
   -- Image --
   -----------

   function Image (Item      : Instant_Vectors.Vector;
                   Separator : String := ", ") return String
   is
      Result : Unbounded_String;
   begin
      for Due_Time of Item loop

         if Result /= Null_Unbounded_String then
            Result := Result & Separator;
         end if;

         Result := Result & Times.Image (Due_Time);
      end loop;

      return To_String (Result);
   end Image;




   procedure Set_Index (Item : in out Deliverable;
                        Idx  : Deliverable_Index)
   is
   begin
      if Item.Index /= No_Index or Item.Is_Clone then
         raise Constraint_Error;
      else
         Item.Index := Node_Index (Idx);

         for Child of Item.Clone_List loop
            Child.Index := Node_Index (Idx);
         end loop;
      end if;
   end Set_Index;



end EU_Projects.Nodes.Timed_Nodes.Deliverables;
