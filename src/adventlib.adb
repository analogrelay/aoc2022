with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Directories;           use Ada.Directories;

package body AdventLib is
   procedure Open_Data_File
     (F : in out Ada.Text_IO.File_Type; Day : Day_Type; Name : String)
   is
   begin
      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Get_Data_Path (Day, Name));
   end Open_Data_File;

   function Get_Data_Path (Day : Day_Type; Name : String) return String is
      Day_String : constant String :=
        Tail (Trim (Day'Image, Left), 2, Pad => '0');
      File_Name  : constant String := Name & "." & "day_" & Day_String;
   begin
      return Compose (Get_Data_Root, File_Name, "txt");
   end Get_Data_Path;

   function Get_Data_Root return String is
   begin
      return Value ("ADVENT_DATA_ROOT", Compose (Current_Directory, "data"));
   end Get_Data_Root;

   procedure Dbg(Message: String) is
   begin
      if Value ("ADVENT_DEBUG", "0") = "1" then
         Put_Line ("DBG: " & Message);
      end if;
   end Dbg;

   function Try_Get_Integer_Line
     (F : in out Ada.Text_IO.File_Type; Value : out Integer) return Boolean
   is
      Line : Unbounded_String;
   begin
      Line := Trim (Get_Line (F), Left);
      if Length(Line) = 0 then
         Value := 0;
         return False;
      else
         Value := Integer'Value (To_String (Line));
         return True;
      end if;
   end Try_Get_Integer_Line;
end AdventLib;
