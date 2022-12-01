with Ada.Text_IO;

package AdventLib is
   type Day_Type is range 0 .. 25;

   procedure Open_Data_File
     (F : in out Ada.Text_IO.File_Type; Day : Day_Type; Name : String);
   function Get_Data_Path (Day : Day_Type; Name : String) return String;
   function Get_Data_Root return String;
   function Try_Get_Integer_Line
     (F : in out Ada.Text_IO.File_Type; Value : out Integer) return Boolean;

   procedure Dbg(Message: String);
end AdventLib;
