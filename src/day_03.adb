with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with AdventLib;                use AdventLib;

procedure Day_03 is
   Invalid_Input : exception;

   type Rucksack is record
      Left  : Unbounded_String;
      Right : Unbounded_String;
   end record;

   function Parse_Rucksack (Input : Unbounded_String) return Rucksack is
      Mid : Natural := 0;
   begin
      if Length (Input) mod 2 /= 0 then
         raise Invalid_Input with "Line is an odd number of characters long!";
      end if;
      Mid := Length (Input) / 2;
      return (Left => Head (Input, Mid), Right => Tail (Input, Mid));
   end Parse_Rucksack;

   function Get_Priority (Item : Character) return Natural is
      Pos : constant Natural := Character'Pos (Item);
   begin
      if Pos >= 97 and then Pos <= 122 then
         return Pos - 96;
      elsif Pos >= 65 and then Pos <= 90 then
         return Pos - 38;
      else
         raise Invalid_Input with "Invalid Rucksack item: " & Item;
      end if;
   end Get_Priority;

   procedure Part_1 (Input_Name : String) is
      F      : File_Type;
      Result : Natural := 0;
   begin
      Open_Data_File (F, 3, Input_Name);
      while not End_Of_File (F) loop
         declare
            Line      : Unbounded_String;
            Current   : Rucksack;
            Duplicate : Character := ' ';
            Priority  : Natural;
         begin
            Line    := Get_Line (F);
            Current := Parse_Rucksack (Line);

            --  Now find the duplicate character, the one that's on both sides
            for Left_Index in 1 .. Length (Current.Left) loop
               declare
                  Candidate : constant Character :=
                    Element (Current.Left, Left_Index);
               begin
                  Duplicate := Candidate;
                  exit when Ada.Strings.Unbounded.Count
                      (Current.Right, To_Set (Candidate)) >
                    0;
               end;
            end loop;

            Priority := Get_Priority (Duplicate);
            Result   := Result + Priority;
         end;
      end loop;

      Put_Line ("Part 1 " & Input_Name & " result: " & Integer'Image (Result));

      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      F       : File_Type;
      Group   : array (0 .. 2) of Unbounded_String;
      Counter : Natural := 0;
      Result: Natural := 0;
   begin
      Open_Data_File (F, 3, Input_Name);
      while not End_Of_File (F) loop
         declare
            Group_Idx : Natural   := Counter mod 3;
            Match     : Character := ' ';
         begin
            --  Load into the group
            Group (Group_Idx) := Get_Line (F);

            if Group_Idx = 2 then
               --  Find the character common to all three
               for Char_Index in 1 .. Length (Group (0)) loop
                  declare
                     Candidate : constant Character :=
                       Element (Group (0), Char_Index);
                  begin
                     if Ada.Strings.Unbounded.Count
                         (Group (1), To_Set (Candidate)) >
                       0
                       and then
                         Ada.Strings.Unbounded.Count
                           (Group (2), To_Set (Candidate)) >
                         0
                     then
                        Match := Candidate;
                        exit;
                     end if;
                  end;
               end loop;

               Result   := Result + Get_Priority (Match);
            end if;

            Counter := Counter + 1;
         end;
      end loop;

      Put_Line ("Part 2 " & Input_Name & " result: " & Integer'Image (Result));

      Close (F);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 03");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
   Part_2 ("input");
end Day_03;
