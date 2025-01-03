--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Binary_Insertion_Sort is

   procedure Sort (The_Items : in out Items) is
      Temporary_Item : Item;
      Left_Index     : Index;
      Middle_Index   : Index;
      Right_Index    : Index;
   begin
      for Outer_Index in Index'Succ (The_Items'First) .. The_Items'Last loop
         Temporary_Item := The_Items (Outer_Index);
         Left_Index     := The_Items'First;
         Right_Index    := Outer_Index;
         while Left_Index <= Right_Index loop
            Middle_Index :=
              Index'Val
                ((Index'Pos (Left_Index) + Index'Pos (Right_Index)) / 2);
            if Temporary_Item < The_Items (Middle_Index) then
               exit when (Middle_Index = The_Items'First);
               Right_Index := Index'Pred (Middle_Index);
            else
               exit when (Middle_Index = Outer_Index);
               Left_Index := Index'Succ (Middle_Index);
            end if;
         end loop;
         if Left_Index /= Outer_Index then
            The_Items (Index'Succ (Left_Index) .. Outer_Index) :=
              The_Items (Left_Index .. Index'Pred (Outer_Index));
            The_Items (Left_Index) := Temporary_Item;
         end if;
      end loop;
   end Sort;

end Booch_Light.Binary_Insertion_Sort;

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
--  DEALINGS IN THE SOFTWARE.
