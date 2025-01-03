--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Quick_Sort is

   procedure Exchange
     (Left  : in out Item;
      Right : in out Item)
   is
      Temporary_Item : Item;
   begin
      Temporary_Item := Left;
      Left           := Right;
      Right          := Temporary_Item;
   end Exchange;

   procedure Sort (The_Items : in out Items) is
      procedure Sort_Recursive
        (Left_Index  : in Index;
         Right_Index : in Index)
      is
         Pivot_Item   : Item;
         The_Front    : Index;
         The_Back     : Index;
         Middle_Index : Index;
      begin
         if Left_Index < Right_Index then
            Middle_Index :=
              Index'Val
                ((Index'Pos (Left_Index) + Index'Pos (Right_Index)) / 2);
            if The_Items (Middle_Index) < The_Items (Left_Index) then
               Exchange (The_Items (Middle_Index), The_Items (Left_Index));
            end if;
            if The_Items (Right_Index) < The_Items (Left_Index) then
               Exchange (The_Items (Right_Index), The_Items (Left_Index));
            end if;
            if The_Items (Right_Index) < The_Items (Middle_Index) then
               Exchange (The_Items (Right_Index), The_Items (Middle_Index));
            end if;
            Pivot_Item := The_Items (Middle_Index);
            Exchange
              (The_Items (Middle_Index), The_Items (Index'Pred (Right_Index)));
            The_Front := Index'Succ (Left_Index);
            The_Back  := Index'Pred (Right_Index);
            if The_Back /= The_Items'First then
               The_Back := Index'Pred (The_Back);
            end if;
            loop
               while The_Items (The_Front) < Pivot_Item loop
                  The_Front := Index'Succ (The_Front);
               end loop;
               while Pivot_Item < The_Items (The_Back) loop
                  The_Back := Index'Pred (The_Back);
               end loop;
               if The_Front <= The_Back then
                  if (The_Front = The_Items'Last)
                    or else (The_Back = The_Items'First)
                  then
                     return;
                  else
                     Exchange (The_Items (The_Front), The_Items (The_Back));
                     The_Front := Index'Succ (The_Front);
                     The_Back  := Index'Pred (The_Back);
                  end if;
               end if;
               exit when The_Front > The_Back;
            end loop;
            Sort_Recursive (Left_Index, The_Back);
            Sort_Recursive (The_Front, Right_Index);
         end if;
      end Sort_Recursive;
   begin
      Sort_Recursive (The_Items'First, The_Items'Last);
   end Sort;

end Booch_Light.Quick_Sort;

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
