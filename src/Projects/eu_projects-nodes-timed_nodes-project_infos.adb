pragma Ada_2012;
with Ada.Finalization; use Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;
package body Eu_Projects.Nodes.Timed_Nodes.Project_Infos is

   ------------
   -- Create --
   ------------

   function Create (Label        : Node_Label;
                    Name         : String;
                    Short_Name   : String;
                    Stop_At      : String;
                    Node_Dir     : in out Node_Tables.Node_Table)
                    return Info_Access
   is
      Result : Info_Access;
   begin
      Result := new Project_Info'(Controlled with
                                    Class         => Info_Node,
                                  Label         => Label,
                                  Name          => To_Unbounded_String (Name),
                                  Short_Name    => To_Unbounded_String (Short_Name),
                                  Index         => No_Index,
                                  Description   => Null_Unbounded_String,
                                  Attributes    => Attribute_Maps.Empty_Map,
                                  Expected_Raw      => To_Unbounded_String (Stop_At),
                                  Expected_Symbolic => <>,
                                  Expected_On       => <>,
                                  Fixed             => False,
                                  WPs               => Node_Label_Lists.Empty_Vector);

      Node_Dir.Insert (ID   => Label,
                       Item => Node_Access (Result));

      return Result;
   end Create;

   ------------
   -- Add_WP --
   ------------

   procedure Add_WP (Info : in out Project_Info;
                     WP   : Node_Label)
   is
   begin
      Info.WPs.Append (WP);
   end Add_WP;


end Eu_Projects.Nodes.Timed_Nodes.Project_Infos;
