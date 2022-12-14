with Ada.Text_IO;    use Ada.Text_IO;
with AdventLib;      use AdventLib;

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
      F              : File_Type;
      Running        : Integer := 0;
      Sum            : Integer := 0;
      Max: Integer := 0;
      Second_Max: Integer := 0;
      Third_Max: Integer := 0;
   begin
      Open_Data_File (F, 1, Input_Name);

      --  Load the calorie counts into a vector
      while not End_Of_File (F) loop
         declare
            Current : Integer;
         begin
            if Try_Get_Integer_Line (F, Current) then
               Running := Running + Current;
            else
               if Running > Max then
                  Third_Max := Second_Max;
                  Second_Max := Max;
                  Max := Running;
               elsif Running > Second_Max then
                  Third_Max := Second_Max;
                  Second_Max := Running;
               elsif Running > Third_Max then
                  Third_Max := Running;
               end if;
               Running := 0;
            end if;
         end;
      end loop;

      --  Find the largest 3 items and sum them
      Sum := Max + Second_Max + Third_Max;
      Put_Line ("Part 2 " & Input_Name & " Result: " & Sum'Image);

      Close (F);
   end Part_2;

begin
   Put_Line ("Advent of Code Day 01");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
   Part_2 ("input");
end Day_01;
