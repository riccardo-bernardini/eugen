with Ada.Sequential_IO;
with Ada.Directories;

package body Utilities is


   -----------
   -- Slurp --
   -----------

   function Slurp (Filename : String) return String is

      subtype Content is String (1 .. Integer (Ada.Directories.Size (Filename)));
      Result : Content;

      package Content_IO is new Ada.Sequential_IO (Content);
      Input  : Content_IO.File_Type;

   begin
      Content_IO.Open (File     => Input,
                       Mode     => Content_IO.In_File,
                       Name     => Filename);

      Content_IO.Read (File => Input,
                       Item => Result);

      Content_IO.Close (Input);

      return Result;
   end Slurp;
end Utilities;
