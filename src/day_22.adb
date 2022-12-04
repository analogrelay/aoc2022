with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with AdventLib;                use AdventLib;

procedure Day_22 is
   procedure Part_1 (Input_Name : String) is
      F : File_Type;
      Result: Natural;
   begin
      Open_Data_File (F, 22, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line              : Unbounded_String;
         begin
            Line := Get_Line (F);
         end;
      end loop;

      Put_Line ("Part 1 " & Input_Name & " Result: " & Result'Image);
      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      F : File_Type;
      Result: Natural;
   begin
      Open_Data_File (F, 22, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line              : Unbounded_String;
         begin
            Line := Get_Line (F);
         end;
      end loop;

      Put_Line ("Part 2 " & Input_Name & " Result: " & Result'Image);
      Close (F);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 22");
   --  Part_1 ("test");
   --  Part_1 ("input");
   --  Part_2 ("test");
   --  Part_2 ("input");
end Day_22;
