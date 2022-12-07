with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Containers.Vectors;
with AdventLib;                use AdventLib;

procedure Day_07 is
   Input_Error : exception;

   type Terminal_Line_Kind is (Change_Directory, List, File, Directory);

   type Terminal_Line is record
      Kind : Terminal_Line_Kind;
      Name : Unbounded_String;
      Size : Natural;
   end record;

   type Terminal_Line_Access is access Terminal_Line;

   function Describe (Line : Terminal_Line) return String is
   begin
      case Line.Kind is
         when Change_Directory =>
            return "Change_Directory " & To_String (Line.Name);
         when List =>
            return "List";
         when File =>
            return "File " & To_String (Line.Name) & " " & Line.Size'Image;
         when Directory =>
            return "Directory " & To_String (Line.Name);
      end case;
   end Describe;

   Space_Set : Character_Set := To_Set (' ');
   function Parse_Terminal_Line (Input : Unbounded_String) return Terminal_Line
   is
   begin
      if Length (Input) < 1 then
         raise Input_Error
           with "Invalid terminal line: '" & To_String (Input) & "'";
      end if;

      if Element (Input, 1) = '$' then
         declare
            Command : String (1 .. 2);
         begin
            Command := Slice (Input, 3, 4);
            if Command = "cd" then
               return
                 (Kind => Change_Directory,
                  Name => Unbounded_Slice (Input, 6, Length (Input)),
                  Size => 0);
            elsif Command = "ls" then
               return
                 (Kind => List, Name => To_Unbounded_String (""), Size => 0);
            else
               raise Input_Error
                 with "Invalid terminal line: '" & To_String (Input) & "'";
            end if;
         end;
      else
         declare
            Space_Idx : Natural;
            Left      : Unbounded_String;
            Right     : Unbounded_String;
         begin
            Space_Idx := Index (Input, Space_Set, 1);
            if Space_Idx = 0 then
               raise Input_Error
                 with "Invalid terminal line: '" & To_String (Input) & "'";
            end if;
            Left  := Unbounded_Slice (Input, 1, Space_Idx - 1);
            Right := Unbounded_Slice (Input, Space_Idx + 1, Length (Input));
            if Left = "dir" then
               return (Kind => Directory, Name => Right, Size => 0);
            else
               return
                 (Kind => File, Name => Right,
                  Size => Natural'Value (To_String (Left)));
            end if;
         end;
      end if;

      raise Input_Error
        with "Invalid terminal line: '" & To_String (Input) & "'";
   end Parse_Terminal_Line;

   type File_System_Entry;
   type File_System_Entry_Access is access File_System_Entry;
   type File_System_Entry_Kind is (File_Entry, Directory_Entry);
   package File_System_Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => File_System_Entry_Access);
   type File_System_Entry is record
      Kind     : File_System_Entry_Kind;
      Name     : Unbounded_String;
      Size     : Natural;
      Children : File_System_Entry_Vectors.Vector;
      Parent   : File_System_Entry_Access := null;
   end record;

   procedure Print_File_System_Entry
     (The_Entry : File_System_Entry_Access; Indent : Natural := 0)
   is
   begin
      for I in 1 .. Indent loop
         Put ("  ");
      end loop;
      case The_Entry.Kind is
         when File_Entry =>
            Put_Line
              ("File " & To_String (The_Entry.Name) & " " &
               The_Entry.Size'Image);
         when Directory_Entry =>
            Put_Line ("Directory " & To_String (The_Entry.Name));
            for I in
              The_Entry.Children.First_Index .. The_Entry.Children.Last_Index
            loop
               Print_File_System_Entry (The_Entry.Children (I), Indent + 1);
            end loop;
      end case;
   end Print_File_System_Entry;

   function Build_File_System(Input_Name: String) return File_System_Entry_Access is
      F      : File_Type;
      Root   : constant File_System_Entry_Access :=
        new File_System_Entry'
          (Kind   => Directory_Entry, Name => To_Unbounded_String ("/"),
           Size   => 0, Children => File_System_Entry_Vectors.Empty_Vector,
           Parent => null);
      Ptr    : File_System_Entry_Access := Root;
   begin
      Open_Data_File (F, 7, Input_Name);

      while not End_Of_File (F) loop
         declare
            Line : Terminal_Line;
         begin
            Line := Parse_Terminal_Line (Get_Line (F));

            --  Handle the line
            case Line.Kind is
               when Change_Directory =>
                  if Line.Name = "/" then
                     Ptr := Root;
                  elsif Line.Name = ".." then
                     if Ptr.Parent = null then
                        raise Input_Error with "Cannot CD to parent of root";
                     end if;
                     Ptr := Ptr.Parent;
                  else
                     declare
                        Found : Boolean := False;
                     begin
                        for I in
                          Ptr.Children.First_Index .. Ptr.Children.Last_Index
                        loop
                           if Ptr.Children (I).Name = Line.Name then
                              Found := True;
                              Ptr   := Ptr.Children (I);
                              exit;
                           end if;
                        end loop;
                        if not Found then
                           raise Input_Error
                             with "Cannot CD to unknown directory: " &
                             To_String (Line.Name);
                        end if;
                     end;
                  end if;
               when File =>
                  Ptr.Children.Append
                    (new File_System_Entry'
                       (Kind     => File_Entry, Name => Line.Name,
                        Size     => Line.Size,
                        Children => File_System_Entry_Vectors.Empty_Vector,
                        Parent   => Ptr));
               when Directory =>
                  Ptr.Children.Append
                    (new File_System_Entry'
                       (Kind     => Directory_Entry, Name => Line.Name,
                        Size     => Line.Size,
                        Children => File_System_Entry_Vectors.Empty_Vector,
                        Parent   => Ptr));
               when others =>
                  null;
            end case;
         end;
      end loop;
      Close (F);

      return Root;
   end Build_File_System;

   function Compute_Part_1_Result(Root : File_System_Entry_Access) return Natural is
      My_Size: Natural := 0;
      Total: Natural := 0;
   begin
      -- To start with, get the sizes for each directory within this directory
      for C of Root.Children loop
         if C.Kind = Directory_Entry then
            Total := Total + Compute_Part_1_Result(C);
         end if;

         if C.Size = 0 then
            raise Input_Error with "Directory size should have been computed!";
         end if;
         My_Size := My_Size + C.Size;
      end loop;

      if Root.Kind = Directory_Entry then
         Root.Size := My_Size;
      end if;

      if My_Size <= 100_000 then
         Total := Total + My_Size;
      end if;

      return Total;
   end Compute_Part_1_Result;

   procedure Collect_Candidates_For_Deletion(Root: File_System_Entry_Access; Candidates: in out File_System_Entry_Vectors.Vector) is
      My_Size: Natural := 0;
   begin
      for C of Root.Children loop
         if C.Kind = Directory_Entry then
            Collect_Candidates_For_Deletion(C, Candidates);
         end if;
         My_Size := My_Size + C.Size;
      end loop;

      Root.Size := My_Size;
      Candidates.Append(Root);
   end Collect_Candidates_For_Deletion;

   procedure Part_1 (Input_Name : String) is
      Result : Natural;
      Root: File_System_Entry_Access;
   begin
      Root := Build_File_System(Input_Name);
      Result := Compute_Part_1_Result(Root);
      Put_Line ("Part 1 " & Input_Name & " Result: " & Result'Image);
   end Part_1;

   procedure Part_2 (Input_Name : String) is
      Root: File_System_Entry_Access;
      Candidates: File_System_Entry_Vectors.Vector;
      Min : Natural := (2 ** 31) - 1;
      Required_Size: Natural;
   begin
      Root := Build_File_System(Input_Name);
      Collect_Candidates_For_Deletion(Root, Candidates);

      --  Compute the size we need
      Required_Size := 30_000_000 - (70_000_000 - Root.Size);
      Dbg ("Finding candidate directory of size " & Required_Size'Image);

      for C of Candidates loop
         if C.Size >= Required_Size and then C.Size < Min then
            Min := C.Size;
         end if;
      end loop;
      Put_Line ("Part 2 " & Input_Name & " Result: " & Min'Image);
   end Part_2;
begin
   Put_Line ("Advent of Code Day 07");
   Part_1 ("test");
   Part_1 ("input");
   Part_2 ("test");
   Part_2 ("input");
end Day_07;
