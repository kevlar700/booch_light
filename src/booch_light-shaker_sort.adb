--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Shaker_Sort is

   procedure Sort (The_Items : in out Items) is
      Temporary_Item  : Item;
      Temporary_Index : Index;
      Left_Index      : Index;
      Right_Index     : Index;
   begin
      Left_Index  := Index'Succ (The_Items'First);
      Right_Index := The_Items'Last;
      loop
         for Middle_Index in reverse Left_Index .. Right_Index loop
            if The_Items (Middle_Index) < The_Items (Index'Pred (Middle_Index))
            then
               Temporary_Item := The_Items (Index'Pred (Middle_Index));
               The_Items (Index'Pred (Middle_Index)) :=
                 The_Items (Middle_Index);
               The_Items (Middle_Index)              := Temporary_Item;
               Temporary_Index                       := Middle_Index;
            end if;
         end loop;
         Left_Index := Index'Succ (Temporary_Index);
         for Middle_Index in Left_Index .. Right_Index loop
            if The_Items (Middle_Index) < The_Items (Index'Pred (Middle_Index))
            then
               Temporary_Item := The_Items (Index'Pred (Middle_Index));
               The_Items (Index'Pred (Middle_Index)) :=
                 The_Items (Middle_Index);
               The_Items (Middle_Index)              := Temporary_Item;
               Temporary_Index                       := Middle_Index;
            end if;
         end loop;
         Right_Index := Index'Pred (Temporary_Index);
         exit when Left_Index > Right_Index;
      end loop;
   end Sort;

end Booch_Light.Shaker_Sort;

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
