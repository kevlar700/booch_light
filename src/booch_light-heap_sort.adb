--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Heap_Sort is

   procedure Sort (The_Items : in out Items) is

      Temporary_Item : Item;
      Left_Index     : Index;
      Right_Index    : Index;

      procedure Sift
        (Left_Index  : Index;
         Right_Index : Index)
      is
         Temporary_Item : Item  := The_Items (Left_Index);
         The_Front      : Index := Left_Index;
         The_Back       : Index := Index'Val (Index'Pos (The_Front) * 2);
      begin
         while The_Back <= Right_Index loop
            if The_Back < Right_Index
            then
               if The_Items (The_Back) < The_Items (Index'Succ (The_Back))
               then
                  The_Back := Index'Succ (The_Back);
               end if;
            end if;
            exit when not (Temporary_Item < The_Items (The_Back));
            The_Items (The_Front) := The_Items (The_Back);
            The_Front             := The_Back;
            exit when (Index'Pos (The_Front) * 2 > Index'Pos (The_Items'Last));
            The_Back := Index'Val (Index'Pos (The_Front) * 2);
         end loop;
         The_Items (The_Front) := Temporary_Item;
      end Sift;

   begin
      Left_Index  :=
        Index'Val
          (((Index'Pos (The_Items'Last) - Index'Pos (The_Items'First) + 1) /
            2) +
           1);
      Right_Index := The_Items'Last;
      while Left_Index > The_Items'First loop
         Left_Index := Index'Pred (Left_Index);
         Sift (Left_Index, Right_Index);
      end loop;
      while Right_Index > The_Items'First loop
         Temporary_Item              := The_Items (The_Items'First);
         The_Items (The_Items'First) := The_Items (Right_Index);
         The_Items (Right_Index)     := Temporary_Item;
         Right_Index                 := Index'Pred (Right_Index);
         Sift (Left_Index, Right_Index);
      end loop;
   end Sort;

end Booch_Light.Heap_Sort;

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
