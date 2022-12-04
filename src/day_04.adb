with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with AdventLib;                use AdventLib;
with Ada.Strings.Maps;         use Ada.Strings.Maps;

procedure Day_04 is
   Invalid_Input : exception;

   Dash_Set  : Character_Set := To_Set ('-');
   Comma_Set : Character_Set := To_Set (',');

   type Assignment is record
      Min : Natural;
      Max : Natural;
   end record;

   function Parse_Assignment (Input : Unbounded_String) return Assignment is
      Dash_Index : Natural := 0;
   begin
      Dash_Index := Index (Input, Dash_Set, 1);
      return
        (Min => Integer'Value (To_String (Head (Input, Dash_Index - 1))),
         Max => Integer'Value (To_String (Tail (Input, Length(Input) - Dash_Index))));
   end Parse_Assignment;

   function Left_Contains_Right(Left: Assignment; Right: Assignment) return Boolean is
   begin
      return Left.Min >= Right.Min
         and then Left.Min <= Right.Max
         and then Left.Max >= Right.Min
         and then Left.Max <= Right.Max;
   end Left_Contains_Right;

   function One_Contains_Other(First_Assignment :Assignment; Second_Assignment: Assignment) return Boolean is
   begin
      return Left_Contains_Right(First_Assignment, Second_Assignment) or else
         Left_Contains_Right(Second_Assignment, First_Assignment);
   end One_Contains_Other;

   function Overlaps(Left: Assignment; Right: Assignment) return Boolean is
   begin
      return not (Right.Min > Left.Max or else Left.Min > Right.Max);
   end Overlaps;

   procedure Part_1 (Input_Name : String) is
      F : File_Type;
      Count: Natural := 0;
   begin
      Open_Data_File (F, 4, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line              : Unbounded_String;
            Comma_Index       : Natural := 0;
            First_Assignment  : Assignment;
            Second_Assignment : Assignment;
         begin
            Line := Get_Line (F);

            Comma_Index := Index (Line, Comma_Set, 1);

            First_Assignment  := Parse_Assignment (Head (Line, Comma_Index - 1));
            Second_Assignment :=
              Parse_Assignment (Tail (Line, Length(Line) - Comma_Index));

            if One_Contains_Other(First_Assignment, Second_Assignment) then
               Count := Count + 1;
            end if;
         end;
      end loop;

      Put_Line ("Part 1 " & Input_Name & " Result: " & Count'Image);
      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      F : File_Type;
      Count: Natural := 0;
   begin
      Open_Data_File (F, 4, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line              : Unbounded_String;
            Comma_Index       : Natural := 0;
            First_Assignment  : Assignment;
            Second_Assignment : Assignment;
         begin
            Line := Get_Line (F);

            Comma_Index := Index (Line, Comma_Set, 1);

            First_Assignment  := Parse_Assignment (Head (Line, Comma_Index - 1));
            Second_Assignment :=
              Parse_Assignment (Tail (Line, Length(Line) - Comma_Index));

            if Overlaps(First_Assignment, Second_Assignment) then
               Count := Count + 1;
            end if;
         end;
      end loop;

      Put_Line ("Part 2 " & Input_Name & " Result: " & Count'Image);
      Close (F);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 04");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
   Part_2 ("input");
end Day_04;
