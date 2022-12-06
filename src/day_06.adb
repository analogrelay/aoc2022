with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with AdventLib;                use AdventLib;
with Ada.Containers;           use Ada.Containers;
with Ada.Containers.Ordered_Sets;

procedure Day_06 is
   package Character_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Character);

   function Find_Marker
     (Data_Stream : Unbounded_String; Marker_Length : Natural) return Natural
   is
   begin
      for I in Marker_Length .. Length (Data_Stream) loop
         declare
            Set : Character_Sets.Set;
         begin
            for J in 0 .. Marker_Length - 1 loop
               Set.Include (Element (Data_Stream, I - J));
            end loop;

            if Set.Length = Count_Type (Marker_Length) then
               return I;
            end if;
         end;
      end loop;

      return 0;
   end Find_Marker;

   procedure Part_1 (Input_Name : String) is
      F     : File_Type;
      Index : Natural := 0;
   begin
      Open_Data_File (F, 6, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line         : Unbounded_String;
            Start_Marker : Natural;
         begin
            Line         := Get_Line (F);
            Start_Marker := Find_Marker (Line, 4);
            Put_Line
              ("Part 1 " & Input_Name & " line " & Natural'Image (Index) &
               " start-of-packet at " & Natural'Image (Start_Marker));
            Index := Index + 1;
         end;
      end loop;

      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      F     : File_Type;
      Index : Natural := 0;
   begin
      Open_Data_File (F, 6, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line         : Unbounded_String;
            Start_Marker : Natural;
         begin
            Line         := Get_Line (F);
            Start_Marker := Find_Marker (Line, 14);
            Put_Line
              ("Part 2 " & Input_Name & " line " & Natural'Image (Index) &
               " start-of-message at " & Natural'Image (Start_Marker));
            Index := Index + 1;
         end;
      end loop;

      Close (F);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 06");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
   Part_2 ("input");
end Day_06;
