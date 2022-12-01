with Ada.Text_IO; use Ada.Text_IO;
with AdventLib;   use AdventLib;
with Ada.Containers.Vectors;

procedure Day_01 is
   procedure Part_1 (Input_Name : String) is
      F       : File_Type;
      Running : Integer := 0;
      Best    : Integer := 0;
   begin
      Open_Data_File (F, 1, Input_Name);

      while not End_Of_File (F) loop
         declare
            Current : Integer;
         begin
            if Try_Get_Integer_Line (F, Current) then
               Running := Running + Current;
            else
               Best    := Integer'Max (Best, Running);
               Running := 0;
            end if;
         end;
      end loop;

      Best := Integer'Max (Best, Running);

      Put_Line ("Part 1 " & Input_Name & " Result: " & Best'Image);

      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      package Integer_Vectors is new
         Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);
      package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;
      use Integer_Vectors;
      use Integer_Vectors_Sorting;

      F : File_Type;
      Calorie_Counts: Vector;
      Running: Integer := 0;
   begin
      Open_Data_File (F, 1, Input_Name);

      -- Load the calorie counts into a vector
      while not End_Of_File (F) loop
         declare
            Current : Integer;
         begin
            if Try_Get_Integer_Line (F, Current) then
               Running := Running + Current;
            else
               Calorie_Counts.Append (Running);
               Running := 0;
            end if;
         end;
      end loop;

      -- Find the max 3 items


      Close (F);
   end Part_2;

begin
   Put_Line ("Advent of Code Day 01");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
end Day_01;
