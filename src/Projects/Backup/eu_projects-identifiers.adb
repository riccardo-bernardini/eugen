package body EU_Projects.Identifiers is

   -----------
   -- To_ID --
   -----------

   function To_ID
     (X : String)
      return Identifier
   is
   begin
      if not Is_Valid_Identifier (X) then
         raise Bad_Identifier with X;
      end if;

      return  Identifier'(ID => ID_Names.To_Bounded_String (To_Lower (X)));
   end To_ID;

end EU_Projects.Identifiers;
