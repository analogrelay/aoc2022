with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Containers.Vectors;
with AdventLib;                use AdventLib;

procedure Day_05 is
   package Character_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Character);
   use Character_Vectors;

   package Board_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Character_Vectors.Vector);
   use Board_Vectors;

   function Create_Test_Board return Board_Vectors.Vector is
      Board : Board_Vectors.Vector;
   begin
      Board.Append (('Z' & 'N'));
      Board.Append (('M' & 'C' & 'D'));
      Board.Append (To_Vector ('P', 1));
      return Board;
   end Create_Test_Board;

   Test_Board : constant Board_Vectors.Vector := Create_Test_Board;

   function Create_Input_Board return Board_Vectors.Vector is
      Board : Board_Vectors.Vector;
   begin
      Board.Append (('Q' & 'F' & 'M' & 'R' & 'L' & 'W' & 'C' & 'V'));
      Board.Append (('D' & 'Q' & 'L'));
      Board.Append (('P' & 'S' & 'R' & 'G' & 'W' & 'C' & 'N' & 'B'));
      Board.Append (('L' & 'C' & 'D' & 'H' & 'B' & 'Q' & 'G'));
      Board.Append (('V' & 'G' & 'L' & 'F' & 'Z' & 'S'));
      Board.Append (('D' & 'G' & 'N' & 'P'));
      Board.Append (('D' & 'Z' & 'P' & 'V' & 'F' & 'C' & 'W'));
      Board.Append (('C' & 'P' & 'D' & 'M' & 'S'));
      Board.Append (('Z' & 'N' & 'W' & 'T' & 'V' & 'M' & 'P' & 'C'));
      return Board;
   end Create_Input_Board;

   Input_Board : constant Board_Vectors.Vector := Create_Input_Board;

   type Move is record
      Amount : Natural;
      From   : Natural;
      To     : Natural;
   end record;

   Space_Set : constant Character_Set := To_Set (' ');
   function Parse_Move (Input : Unbounded_String) return Move is
      First_Space_Idx  : Natural;
      Second_Space_Idx : Natural;
      Third_Space_Idx  : Natural;
      Fourth_Space_Idx : Natural;
      Fifth_Space_Idx  : Natural;
      Amount           : Natural;
      From             : Natural;
      To               : Natural;
   begin
      --  Example: 'move 99 from 99 to 99'
      First_Space_Idx  := Index (Input, Space_Set, 1);
      Second_Space_Idx := Index (Input, Space_Set, First_Space_Idx + 1);
      Third_Space_Idx  := Index (Input, Space_Set, Second_Space_Idx + 1);
      Fourth_Space_Idx := Index (Input, Space_Set, Third_Space_Idx + 1);
      Fifth_Space_Idx  := Index (Input, Space_Set, Fourth_Space_Idx + 1);

      Amount :=
        Natural'Value
          (Slice (Input, First_Space_Idx + 1, Second_Space_Idx - 1));
      From   :=
        Natural'Value
          (Slice (Input, Third_Space_Idx + 1, Fourth_Space_Idx - 1));
      To     :=
        Natural'Value
          (To_String (Tail (Input, Length (Input) - Fifth_Space_Idx)));
      return (Amount, From, To);
   end Parse_Move;

   --  Encode the initial state of the test and input here
   --  It's much easier than parsing the header of the input
   --  So our input files only contain the move commands.

   procedure Part_1 (Input_Name : String; Input_Board : Board_Vectors.Vector)
   is
      F         : File_Type;
      Result    : Unbounded_String;
      The_Board : Board_Vectors.Vector;
   begin
      Open_Data_File (F, 5, Input_Name);

      The_Board := Input_Board;

      while not End_Of_File (F) loop
         declare
            Line         : Unbounded_String;
            Current_Move : Move;
         begin
            Line         := Get_Line (F);
            Current_Move := Parse_Move (Line);

            --  Dbg ("Current move: " & To_String (Line));

            --  Execute the move
            declare
               To_Board   : Character_Vectors.Vector :=
                  The_Board (Current_Move.To - 1);
               From_Board : Character_Vectors.Vector :=
                  The_Board (Current_Move.From - 1);
            begin
               for I in 1 .. Current_Move.Amount loop
                     To_Board.Append (From_Board.Last_Element);
                     From_Board.Delete (From_Board.Last_Index);
               end loop;

               The_Board (Current_Move.To - 1)   := To_Board;
               The_Board (Current_Move.From - 1) := From_Board;
            end;
         end;
      end loop;

      --  Read the final board state
      for I in The_Board.First_Index .. The_Board.Last_Index loop
         Append (Result, The_Board (I).Last_Element);
      end loop;

      Put_Line ("Part 1 " & Input_Name & " Result: " & To_String (Result));
      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String; Input_Board: Board_Vectors.Vector) is
      F         : File_Type;
      Result    : Unbounded_String;
      The_Board : Board_Vectors.Vector;
   begin
      Open_Data_File (F, 5, Input_Name);

      The_Board := Input_Board;

      while not End_Of_File (F) loop
         declare
            Line         : Unbounded_String;
            Current_Move : Move;
         begin
            Line         := Get_Line (F);
            Current_Move := Parse_Move (Line);

            --  Dbg ("Current move: " & To_String (Line));

            --  Execute the move
            --  Preserve the order, so first pop everything into a local vector
            declare
               Temp_Board  : Character_Vectors.Vector;
               To_Board    : Character_Vectors.Vector :=
                  The_Board (Current_Move.To - 1);
               From_Board  : Character_Vectors.Vector :=
                  The_Board (Current_Move.From - 1);
            begin
               --  Load everything up into the buffer
               for I in 1 .. Current_Move.Amount loop
                  Temp_Board.Append (From_Board.Last_Element);
                  From_Board.Delete (From_Board.Last_Index);
               end loop;

               --  Reverse the buffer and append it into the destination
               for C in reverse Temp_Board.Iterate loop
                  To_Board.Append (Temp_Board(C));
               end loop;

               --  Update the board
               The_Board (Current_Move.To - 1)   := To_Board;
               The_Board (Current_Move.From - 1) := From_Board;
            end;
         end;
      end loop;

      --  Read the final board state
      for I in The_Board.First_Index .. The_Board.Last_Index loop
         Append (Result, The_Board (I).Last_Element);
      end loop;

      Put_Line ("Part 2 " & Input_Name & " Result: " & To_String (Result));
      Close (F);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 05");
   Part_1 ("test", Test_Board);
   Part_1 ("input", Input_Board);
   Part_2 ("test", Test_Board);
   Part_2 ("input", Input_Board);
end Day_05;
