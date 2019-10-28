with EU_Projects.Nodes;
with EU_Projects.Times.Time_Expressions;
with Eu_Projects.Event_Names;
with EU_Projects.Node_Tables;

package Eu_Projects.Nodes.Timed_Nodes.Project_Infos is
   type Project_Info is new Timed_Node with private;

   type Info_Access is access all Project_Info;

   function Create (Label        : Node_Label;
                    Name         : String;
                    Short_Name   : String;
                    Stop_At      : String;
                    Node_Dir     : in out Node_Tables.Node_Table)
                    return Info_Access;


   procedure Add_WP (Info : in out Project_Info;
                     WP   : Node_Label);

   overriding function Full_Index (Item     : Project_Info;
                                   Prefixed : Boolean) return String;

   overriding function Get_Symbolic_Instant (Item : Project_Info;
                                             Var  : Simple_Identifier)
                                             return Times.Time_Expressions.Symbolic_Instant;

   overriding function Get_Symbolic_Duration (Item : Project_Info;
                                              Var  : Simple_Identifier)
                                              return Times.Time_Expressions.Symbolic_Duration;

   overriding function Dependency_List (Item : Project_Info)
                                        return Node_Label_Lists.Vector;


   overriding function Dependency_Ready_Var (Item : Project_Info) return String
   is ("end");

   function Variables (Item : Project_Info) return Variable_List
   is ((1 => To_Id("end")));

   overriding function Is_A (Item  : Project_Info;
                             Var   : Simple_Identifier;
                             Class : Times.Time_Type)
                             return Boolean
   is (case Class is
          when Times.Instant_Value =>
             Var = "end",
          when Times.Duration_Value =>
             False);
private
   type Project_Info is new Timed_Node
     with
      record
         WPs : Node_Label_Lists.Vector;
      end record;

   overriding function Dependency_List (Item : Project_Info)
                                        return Node_Label_Lists.Vector
   is (Item.Wps);

   overriding function Get_Symbolic_Instant
     (Item   : Project_Info;
      Var    : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Instant
   is (if Var = Event_Names.End_Name then
          Item.Expected_Symbolic

       else
          raise Unknown_Instant_Var);


   overriding function Get_Symbolic_Duration
     (Item   : Project_Info;
      Var    : Simple_Identifier)
      return Times.Time_Expressions.Symbolic_Duration
   is (raise Unknown_Duration_Var);

   overriding function Full_Index (Item     : Project_Info;
                                   Prefixed : Boolean) return String
   is ("");
end Eu_Projects.Nodes.Timed_Nodes.Project_Infos;
