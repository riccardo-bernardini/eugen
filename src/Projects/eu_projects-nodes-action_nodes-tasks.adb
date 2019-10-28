with Ada.Finalization; use Ada.Finalization;
package body EU_Projects.Nodes.Action_Nodes.Tasks is

   ------------
   -- Create --
   ------------

   function Create
     (Label        : Task_Label;
      Name         : String;
      Short_Name   : String;
      Leader       : Partners.Partner_Label;
      Parent_WP    : Node_Access;
      Description  : String;
      Active_When  : Action_Time;
      Depends_On   : Nodes.Node_Label_Lists.Vector;
      Intensity    : Task_Intensity;
      Node_Dir     : in out Node_Tables.Node_Table)
      return Project_Task_Access
   is
      Result : Project_Task_Access;
   begin
      Result := new
        Project_Task'(Controlled with
                        Label             => Node_Label (Label),
                      Raw_Time          => Active_When,
                      Class             => Task_Node,
                      Name              => To_Unbounded_String (Name),
                      Short_Name        => Make_Short_Name (Short_Name, Name),
                      Index             => No_Index,
                      Description       => To_Unbounded_String (Description),
                      Attributes        => Attribute_Maps.Empty_Map,
                      Parent            => Parent_WP,
                      Leader            => Leader,
                      Partner_Effort    => Efforts.Effort_Maps.Empty_Map,
                      Depend_On         => Depends_On,
                      Start_Symbolic    => <>,
                      Stop_Symbolic     => <>,
                      Duration_Symbolic => <>,
                      Start_At          => <>,
                      Stop_At           => <>,
                      Elapsed_Time      => <>,
                      Intensity         => Intensity,
                      Start_Fixed       => False,
                      Stop_Fixed        => False
                     );

      Node_Dir.Insert (ID   => Node_Label (Label),
                       Item => Node_Access (Result));


      Result.Initialize_Efforts (Node_Dir);

      return Result;
   end Create;

   function Value (X : String) return Task_Intensity
   is
      use Ada.Strings.Fixed;
      use Ada.Strings;

      function Basic_Value (S : String) return Float
      is
         Tmp : constant String := Trim (S, Both);
         Dot : constant Natural := Index (S, ".");
      begin
         if Tmp'Length = 0 then
            raise Constraint_Error;
         end if;

         if not (for all Idx in Tmp'Range => (Tmp (Idx) in '0' .. '9') or (Idx = Dot)) then
            raise Constraint_Error;
         end if;

         if Dot = 0 then
            return Float (Integer'Value (Tmp));
         else
            return Float'Value (Tmp);
         end if;
      end Basic_Value;


      Pos : constant Natural := Index (Source  => X,
                                       Pattern => "%");

   begin
      if Pos = 0 then
         return Basic_Value (X);
      else
         if not (for all Idx in Pos + 1 .. X'Last => X (Idx) = ' ') then
            raise Constraint_Error;
         end if;


         return Basic_Value (X (X'First .. Pos - 1)) / 100.0;
      end if;
   end Value;


   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (Tsk : in out Project_Task;
                        Idx : Task_Index)
   is
   begin
      Tsk.Index := Node_Index (Idx);
   end Set_Index;


   --------------------
   -- Add_Dependence --
   --------------------

   procedure Add_Dependence
     (Item : in out Project_Task;
      Dep  : in     Task_Label)
   is
   begin
      Item.Depend_On.Append (Node_Label (Dep));
   end Add_Dependence;



end EU_Projects.Nodes.Action_Nodes.Tasks;
