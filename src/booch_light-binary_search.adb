--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.
with Booch_Light.Alogs;

package body Booch_Light.Binary_Search is

   procedure Location_Of
     (The_Key      :     Key;
      In_The_Items :     Items;
      The_Location : out Index;
      Booch_Status : out Status.Location_Of)
   is
      Lower_Index : Index := In_The_Items'First;
      Upper_Index : Index := In_The_Items'Last;
   begin
      while Lower_Index <= Upper_Index loop
         The_Location :=
           Index'Val ((Index'Pos (Lower_Index) + Index'Pos (Upper_Index)) / 2);
         if Is_Equal (The_Key, In_The_Items (The_Location))
         then
            Booch_Status := OK;
            return;
         elsif Is_Less_Than (The_Key, In_The_Items (The_Location))
         then
            exit when (The_Location = In_The_Items'First);
            Upper_Index := Index'Pred (The_Location);
         else
            exit when (The_Location = In_The_Items'Last);
            Lower_Index := Index'Succ (The_Location);
         end if;
      end loop;

      The_Location := Index'Last;
      Booch_Status := Item_Not_Found;
      Alogs.Log
        (Log_ID  => "C4215680AF21A1F7",
         Message => "Item_Not_Found: Binary search failed");

   end Location_Of;

end Booch_Light.Binary_Search;

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
