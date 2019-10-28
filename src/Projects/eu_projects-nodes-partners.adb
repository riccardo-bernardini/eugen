with Ada.Finalization; use Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;

package body EU_Projects.Nodes.Partners is


function Create (ID         : Partner_Label;
                    Name       : String;
                    Short_Name : String;
                    Country    : Country_Code;
                    Node_Dir   : in out Node_Tables.Node_Table)
                    return Partner_Access
   is
      Result : Partner_Access;
   begin
      result := new
        Partner'(Controlled with
                   Label           => Node_Label (ID),
                 Name            => To_Unbounded_String (Name),
                 Short_Name      => To_Unbounded_String (Short_Name),
                 Index           => No_Partner,
                 Class           => Partner_Node,
                 Description     => Null_Unbounded_String,
                 Attributes      => Attribute_Maps.Empty_Map,
                 Personnel_Costs => Personnel_Cost_Vectors.Empty_Vector,
                 Other_Costs     => Other_Cost_Vectors.Empty_Vector,
                 Country         => Country);

--        Put_Line ("<<INSERT:" & To_String (Id) & ">>");
      Node_Dir.Insert (ID   => Node_Label (ID),
                       Item => Node_Access (Result));

      return Result;
   end Create;



   --------------
   -- Add_Role --
   --------------

   procedure Add_Role (To           : in out Partner;
                       Role         : in Role_Name;
                       Description  : in String;
                       Monthly_Cost : in Currency)
   is
   begin
      To.Personnel_Costs.Append
        (Personnel_Cost'(Role        => Role,
                         Monthly_Cost => Monthly_Cost,
                         Description  => To_Unbounded_String (Description)));
   end Add_Role;

   -----------------------------
   -- Add_Undivisible_Expense --
   -----------------------------

   procedure Add_Undivisible_Expense (To          : in out Partner;
                                      Description : in String;
                                      Cost        : in Currency)
   is
   begin
      To.Other_Costs.Append
        (Expense'(Divisible   => False,
                  Description => To_Unbounded_String (Description),
                  Cost        => Cost));
   end Add_Undivisible_Expense;

   ---------------------------
   -- Add_Divisible_Expense --
   ---------------------------

   procedure Add_Divisible_Expense (To          : in out Partner;
                                    Description : in String;
                                    Unit_Cost   : in Currency;
                                    Amount      : in Natural)
   is

   begin
      To.Other_Costs.Append
        (Expense'(Divisible   => True,
                  Description => To_Unbounded_String (Description),
                  Unit_Cost   => Unit_Cost,
                  Amount      => Amount));
   end Add_Divisible_Expense;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (Item : in out Partner;
                        Idx  : Partner_Index)
   is
   begin
      if Item.Index /= No_Index and Item.Index /= Node_Index (Idx) then
         raise Constraint_Error;
      else
         Item.Index := Node_Index (Idx);
      end if;
   end Set_Index;

end EU_Projects.Nodes.Partners;
