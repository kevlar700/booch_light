--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Natural_Merge_Sort is

   procedure Sort
     (The_File         : in out File;
      Temporary_File_1 : in out File;
      Temporary_File_2 : in out File;
      Booch_Status     :    out Locus.Sort)
   is

      Number_Of_Runs : Natural;

      procedure Copy
        (From_The_File : in out File;
         To_The_File   : in out File;
         End_Of_Run    :    out Boolean)
      is
         Temporary_Item : Item;
      begin
         Get (From_The_File, Temporary_Item);
         Put (To_The_File, Temporary_Item);
         if Is_End_Of_File (From_The_File) then
            End_Of_Run := True;
         else
            End_Of_Run := (Next_Item (From_The_File) < Temporary_Item);
         end if;
      end Copy;

      procedure Copy_Run
        (From_The_File : in out File;
         To_The_File   : in out File)
      is
         End_Of_Run : Boolean;
      begin
         loop
            Copy (From_The_File, To_The_File, End_Of_Run);
            exit when End_Of_Run;
         end loop;
      end Copy_Run;

      procedure Merge_Run
        (From_The_File : in out File;
         And_The_File  : in out File;
         To_The_File   : in out File)
      is
         End_Of_Run : Boolean;
      begin
         loop
            if not (Next_Item (And_The_File) < Next_Item (From_The_File)) then
               Copy (From_The_File, To_The_File, End_Of_Run);
               if End_Of_Run then
                  Copy_Run (And_The_File, To_The_File);
                  exit;
               end if;
            else
               Copy (And_The_File, To_The_File, End_Of_Run);
               if End_Of_Run then
                  Copy_Run (From_The_File, To_The_File);
                  exit;
               end if;
            end if;
         end loop;
      end Merge_Run;

   begin
      loop
         Open_For_Reading (The_File);
         if Is_End_Of_File (The_File) then
            Close (The_File);
            Close (Temporary_File_1);
            Close (Temporary_File_2);
            Booch_Status := File_Is_Empty;
            Alterable_Log.Log
              (Log_ID  => "44EAD0CA81130844",
               Message => "File_Is_Empty: Natural_Merge Sort failed");
            Booch_Status := Position_Error;
            return;
         else
            Open_For_Writing (Temporary_File_1);
            Open_For_Writing (Temporary_File_2);
            loop
               Copy_Run
                 (The_File,
                  To_The_File => Temporary_File_1);
               if not Is_End_Of_File (The_File) then
                  Copy_Run
                    (The_File,
                     To_The_File => Temporary_File_2);
               end if;
               exit when Is_End_Of_File (The_File);
            end loop;
            Open_For_Writing (The_File);
            Open_For_Reading (Temporary_File_1);
            Open_For_Reading (Temporary_File_2);
            Number_Of_Runs := 0;
            while (not Is_End_Of_File (Temporary_File_1))
              and then (not Is_End_Of_File (Temporary_File_2))
            loop
               Merge_Run
                 (Temporary_File_1,
                  Temporary_File_2,
                  To_The_File => The_File);
               Number_Of_Runs := Number_Of_Runs + 1;
            end loop;
            while not Is_End_Of_File (Temporary_File_1) loop
               Copy_Run
                 (Temporary_File_1,
                  To_The_File => The_File);
               Number_Of_Runs := Number_Of_Runs + 1;
            end loop;
            while not Is_End_Of_File (Temporary_File_2) loop
               Copy_Run
                 (Temporary_File_2,
                  To_The_File => The_File);
               Number_Of_Runs := Number_Of_Runs + 1;
            end loop;
            exit when (Number_Of_Runs = 1);
         end if;
      end loop;
      Close (The_File);
      Close (Temporary_File_1);
      Close (Temporary_File_2);

      Booch_Status := OK;
   end Sort;

end Booch_Light.Natural_Merge_Sort;

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
