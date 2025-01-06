--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Radix_Sort is

   procedure Sort (The_Items : in out Items) is
      procedure Sort_Recursive
        (Left_Index  : in Index;
         Right_Index : in Index;
         Bit         : in Positive)
      is
         Temporary_Left  : Index;
         Temporary_Right : Index;
         Temporary_Item  : Item;
      begin
         if Right_Index > Left_Index then
            Temporary_Left  := Left_Index;
            Temporary_Right := Right_Index;
            loop
               while (not Bit_Of (The_Items (Temporary_Left), Bit))
                 and then (Temporary_Left < Temporary_Right)
               loop
                  Temporary_Left := Index'Succ (Temporary_Left);
               end loop;
               while (Bit_Of (The_Items (Temporary_Right), Bit))
                 and then (Temporary_Left < Temporary_Right)
               loop
                  Temporary_Right := Index'Pred (Temporary_Right);
               end loop;
               Temporary_Item              := The_Items (Temporary_Left);
               The_Items (Temporary_Left)  := The_Items (Temporary_Right);
               The_Items (Temporary_Right) := Temporary_Item;
               exit when (Temporary_Left = Temporary_Right);
            end loop;
            if not Bit_Of (The_Items (Right_Index), Bit) then
               Temporary_Right := Index'Succ (Temporary_Right);
            end if;
            if Bit < Number_Of_Key_Bits then
               if Temporary_Right > The_Items'First then
                  Sort_Recursive
                    (Left_Index, Index'Pred (Temporary_Right), Bit + 1);
               end if;
               Sort_Recursive (Temporary_Right, Right_Index, Bit + 1);
            end if;
         end if;
      end Sort_Recursive;

   begin
      Sort_Recursive (The_Items'First, The_Items'Last, 1);
   end Sort;

end Booch_Light.Radix_Sort;

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
