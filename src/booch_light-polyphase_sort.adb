--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Polyphase_Sort is

   procedure Sort
     (The_File        : in out File;
      Temporary_Files : in out Files;
      Sorted_File     :    out Positive;
      Booch_Status    :    out Locus.Sort)
   is

      Number_Of_Runs            : array (1 .. Number_Of_Files) of Natural;
      Number_Of_Dummy_Runs      : array (1 .. Number_Of_Files) of Natural;
      Last_Item                 : array (1 .. Number_Of_Files) of Item;
      File_Map                  : array (1 .. Number_Of_Files) of Positive;
      Available_Files           : array (1 .. Number_Of_Files) of Positive;
      Level                     : Natural := 1;
      Output_File               : Natural := 1;
      Number_Of_Available_Files : Natural;
      Last_File                 : Positive;
      Last_Runs                 : Natural;
      Last_Dummy_Runs           : Natural;

      procedure Select_File is
         Temporary_Run : Natural;
      begin
         if Number_Of_Dummy_Runs (Output_File) <
           Number_Of_Dummy_Runs (Output_File + 1)
         then
            Output_File := Output_File + 1;
         else
            if Number_Of_Dummy_Runs (Output_File) = 0 then
               Level         := Level + 1;
               Temporary_Run := Number_Of_Runs (1);
               for Index in 1 .. (Number_Of_Files - 1) loop
                  Number_Of_Dummy_Runs (Index) :=
                    Temporary_Run + Number_Of_Runs (Index + 1) -
                    Number_Of_Runs (Index);
                  Number_Of_Runs (Index)       :=
                    Temporary_Run + Number_Of_Runs (Index + 1);
               end loop;
            end if;
            Output_File := 1;
         end if;
         Number_Of_Dummy_Runs (Output_File) :=
           Number_Of_Dummy_Runs (Output_File) - 1;
      end Select_File;

      procedure Copy_Run is
         Temporary_Item : Item;
      begin
         loop
            Get (The_File, Temporary_Item);
            Put (Temporary_Files (Output_File), Temporary_Item);
            exit when
              (Is_End_Of_File (The_File)
               or else (Next_Item (The_File) < Temporary_Item));
         end loop;
         Last_Item (Output_File) := Temporary_Item;
      end Copy_Run;

      procedure Merge_Run is
         File_Index     : Positive;
         Smallest_Item  : Item;
         Smallest_File  : Positive;
         Temporary_Item : Item;
         End_Of_File    : Boolean;
      begin
         loop
            Number_Of_Available_Files := 0;
            for Index in 1 .. (Number_Of_Files - 1) loop
               if Number_Of_Dummy_Runs (Index) > 0 then
                  Number_Of_Dummy_Runs (Index) :=
                    Number_Of_Dummy_Runs (Index) - 1;
               else
                  Number_Of_Available_Files := Number_Of_Available_Files + 1;
                  Available_Files (Number_Of_Available_Files) :=
                    File_Map (Index);
               end if;
            end loop;
            if Number_Of_Available_Files = 0 then
               Number_Of_Dummy_Runs (Number_Of_Files) :=
                 Number_Of_Dummy_Runs (Number_Of_Files) + 1;
            else
               loop
                  File_Index    := 1;
                  Smallest_File := 1;
                  Smallest_Item :=
                    Next_Item (Temporary_Files (Available_Files (1)));
                  while File_Index < Number_Of_Available_Files loop
                     File_Index     := File_Index + 1;
                     Temporary_Item :=
                       Next_Item
                         (Temporary_Files (Available_Files (File_Index)));
                     if Temporary_Item < Smallest_Item then
                        Smallest_Item := Temporary_Item;
                        Smallest_File := File_Index;
                     end if;
                  end loop;
                  Get
                    (Temporary_Files (Available_Files (Smallest_File)),
                     Temporary_Item);
                  End_Of_File :=
                    Is_End_Of_File
                      (Temporary_Files (Available_Files (Smallest_File)));
                  Put
                    (Temporary_Files (File_Map (Number_Of_Files)),
                     Temporary_Item);
                  if End_Of_File
                    or else
                    (Next_Item
                       (Temporary_Files (Available_Files (Smallest_File))) <
                     Temporary_Item)
                  then
                     Available_Files (Smallest_File) :=
                       Available_Files (Number_Of_Available_Files);
                     Number_Of_Available_Files       :=
                       Number_Of_Available_Files - 1;
                  end if;
                  exit when (Number_Of_Available_Files = 0);
               end loop;
            end if;
            Last_Runs := Last_Runs - 1;
            exit when (Last_Runs = 0);
         end loop;
      end Merge_Run;

   begin
      for Index in 1 .. (Number_Of_Files - 1) loop
         Number_Of_Runs (Index)       := 1;
         Number_Of_Dummy_Runs (Index) := 1;
         Open_For_Writing (Temporary_Files (Index));
      end loop;
      Number_Of_Runs (Number_Of_Files)       := 0;
      Number_Of_Dummy_Runs (Number_Of_Files) := 0;
      Open_For_Reading (The_File);
      if Is_End_Of_File (The_File) then
         for Index in 1 .. Number_Of_Files loop
            Close (Temporary_Files (Index));
         end loop;
         Close (The_File);
         Booch_Status := File_Is_Empty;
         Sorted_File  := Positive'Invalid_Value;
                  Alterable_Log.Log
           (Log_ID  => "98A32881831DE00C",
            Message => "File_Is_Empty: Polyphase Sort failed");
         Booch_Status := Position_Error;
         return;
      else
         loop
            Select_File;
            Copy_Run;
            exit when
              (Is_End_Of_File (The_File)
               or else (Output_File = (Number_Of_Files - 1)));
         end loop;
         while not Is_End_Of_File (The_File) loop
            Select_File;
            if not (Next_Item (The_File) < Last_Item (Output_File)) then
               Copy_Run;
               if Is_End_Of_File (The_File) then
                  Number_Of_Dummy_Runs (Output_File) :=
                    Number_Of_Dummy_Runs (Output_File) + 1;
               else
                  Copy_Run;
               end if;
            else
               Copy_Run;
            end if;
         end loop;
         Close (The_File);
         for Index in 1 .. (Number_Of_Files - 1) loop
            Open_For_Reading (Temporary_Files (Index));
         end loop;
         for Index in 1 .. Number_Of_Files loop
            File_Map (Index) := Index;
         end loop;
         loop
            Last_Runs := Number_Of_Runs (Number_Of_Files - 1);
            Number_Of_Dummy_Runs (Number_Of_Files) := 0;
            Open_For_Writing (Temporary_Files (File_Map (Number_Of_Files)));
            Merge_Run;
            Open_For_Reading (Temporary_Files (File_Map (Number_Of_Files)));
            Last_File       := File_Map (Number_Of_Files);
            Last_Dummy_Runs := Number_Of_Dummy_Runs (Number_Of_Files);
            Last_Runs       := Number_Of_Runs (Number_Of_Files - 1);
            for Index in reverse 2 .. Number_Of_Files loop
               File_Map (Index)             := File_Map (Index - 1);
               Number_Of_Runs (Index)       :=
                 Number_Of_Runs (Index - 1) - Last_Runs;
               Number_Of_Dummy_Runs (Index) :=
                 Number_Of_Dummy_Runs (Index - 1);
            end loop;
            File_Map (1)             := Last_File;
            Number_Of_Runs (1)       := Last_Runs;
            Number_Of_Dummy_Runs (1) := Last_Dummy_Runs;
            Level                    := Level - 1;
            exit when (Level = 0);
         end loop;
         for Index in 1 .. Number_Of_Files loop
            Close (Temporary_Files (Index));
         end loop;
         Sorted_File := File_Map (1);
      end if;

      Booch_Status := OK;

   end Sort;

end Booch_Light.Polyphase_Sort;

--              Original Booch Components (Ada 83 version)
--  License: MIT
--  Copyright (C) 1987 Grady Booch Copyright (C) 2024 Kevin Chadwick (Light
--  runtime compatibility)
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of this software and associated documentation files (the
--  “Software”), to deal in the Software without restriction, including
--  without limitation the rights to use, copy, modify, merge, publish,
--  distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to
--  the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
--  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
--  DEALINGS IN THE SOFTWARE. DEALINGS IN THE SOFTWARE.
