with Ada.Text_IO; use Ada.Text_IO;
with AdventLib;   use AdventLib;

procedure Day_02 is
   type Move is (Rock, Paper, Scissors, Invalid);
   type Intended_Outcome is (Win, Lose, Draw, Invalid);

   function Parse_Outcome (Input : Character) return Intended_Outcome is
   begin
      return
        (case Input is when 'X' => Lose, when 'Y' => Draw, when 'Z' => Win,
           when others => Invalid);
   end Parse_Outcome;

   function Parse_Move (Input : Character) return Move is
   begin
      return
        (case Input is when 'A' | 'X' => Rock, when 'B' | 'Y' => Paper,
           when 'C' | 'Z' => Scissors, when others => Invalid);
   end Parse_Move;

   function Score_Round (My_Move : Move; Their_Move : Move) return Natural is
      Win_Lose_Points : Integer := 0;
      Move_Points     : Integer := 0;
   begin
      if (My_Move = Rock and then Their_Move = Scissors)
        or else (My_Move = Paper and then Their_Move = Rock)
        or else (My_Move = Scissors and then Their_Move = Paper)
      then
         --  I win
         Win_Lose_Points := 6;
      elsif (Their_Move = Rock and then My_Move = Scissors)
        or else (Their_Move = Paper and then My_Move = Rock)
        or else (Their_Move = Scissors and then My_Move = Paper)
      then
         --  I lose
         Win_Lose_Points := 0;
      else
         --  We draw
         Win_Lose_Points := 3;
      end if;

      Move_Points :=
        (case My_Move is when Rock => 1, when Paper => 2, when Scissors => 3,
           when others => 0);

      return Win_Lose_Points + Move_Points;
   end Score_Round;

   procedure Part_1 (Input_Name : String) is
      F           : File_Type;
      Total_Score : Integer := 0;
   begin
      Open_Data_File (F, 2, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line       : String (1 .. 5) := "     ";
            Last       : Natural         := 0;
            Their_Move : Move            := Invalid;
            My_Move    : Move            := Invalid;
         begin
            Get_Line (F, Line, Last);
            if Last < 3 then
               Put_Line
                 ("Invalid input. Line is " & Integer'Image (Last) &
                  " characters long");
               return;
            end if;
            Their_Move := Parse_Move (Line (1));
            My_Move    := Parse_Move (Line (3));

            Total_Score := Total_Score + Score_Round (My_Move, Their_Move);
         end;
      end loop;

      Put_Line
        ("Part 1 " & Input_Name & " result: " & Integer'Image (Total_Score));

      Close (F);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      F           : File_Type;
      Total_Score : Integer := 0;
   begin
      Open_Data_File (F, 2, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line       : String (1 .. 5)  := "     ";
            Last       : Natural          := 0;
            Their_Move : Move             := Invalid;
            My_Move    : Move             := Invalid;
            Outcome    : Intended_Outcome := Invalid;
         begin
            Get_Line (F, Line, Last);
            if Last < 3 then
               Put_Line
                 ("Invalid input. Line is " & Integer'Image (Last) &
                  " characters long");
               return;
            end if;
            Their_Move := Parse_Move (Line (1));
            Outcome    := Parse_Outcome (Line (3));

            --  Determine my move based on intended outcome
            if Outcome = Win then
               My_Move :=
                 (case Their_Move is when Rock => Paper,
                    when Paper => Scissors, when Scissors => Rock,
                    when others => Invalid);
            elsif Outcome = Lose then
               My_Move :=
                 (case Their_Move is when Rock => Scissors, when Paper => Rock,
                    when Scissors => Paper, when others => Invalid);
            else
               My_Move := Their_Move;
            end if;

            Total_Score := Total_Score + Score_Round (My_Move, Their_Move);
         end;
      end loop;

      Put_Line
        ("Part 2 " & Input_Name & " result: " & Integer'Image (Total_Score));

      Close (F);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 02");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
   Part_2 ("input");
end Day_02;
