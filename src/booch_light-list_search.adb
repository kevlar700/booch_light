--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.List_Search is

   procedure Position_Of
     (The_Item     :     Item;
      In_The_List  :     List;
      Position     : out Positive;
      Booch_Status : out Locus.Position_Of)
   is
      Index : List := In_The_List;
   begin
      while not Is_Null (Index) loop
         if The_Item = Head_Of (Index)
         then
            Position     := 1;
            Booch_Status := OK;
            return;
         else
            Index        := Tail_Of (Index);
            Position     := Position + 1;
            Booch_Status := OK;

         end if;
      end loop;

      Alogs.Log
        (Log_ID  => "4EFBD2076988B23D",
         Message => "Item_Not_Found: Position_Of failed");
      Booch_Status := Item_Not_Found;

   end Position_Of;

   procedure Location_Of
     (The_Position :     Positive;
      In_The_List  :     List;
      Location     : out List;
      Booch_Status : out Locus.Location_Of)
   is
      Index : List;
   begin
      if Is_Null (In_The_List)
      then
         Booch_Status := Position_Error;
         return;
      else
         Index := In_The_List;
         for Count in 2 .. The_Position loop
            Index := Tail_Of (Index);
            if Is_Null (Index)
            then
               Alogs.Log
                 (Log_ID  => "01984CA10949A0C5",
                  Message => "Position_Error: Location_Of failed");
               Booch_Status := Position_Error;
               return;
            end if;
         end loop;

         Location     := Index;
         Booch_Status := OK;
         return;
      end if;
   end Location_Of;

   procedure Location_Of
     (The_Item     :     Item;
      In_The_List  :     List;
      Location     : out List;
      Booch_Status : out Locus.Location_Of_Item)
   is
      Index : List := In_The_List;
   begin
      while not Is_Null (Index) loop
         if The_Item = Head_Of (Index)
         then
            Location     := Index;
            Booch_Status := OK;
            return;
         else
            Index := Tail_Of (Index);
         end if;
      end loop;

      Alogs.Log
        (Log_ID  => "76AA1ED3B13735F4",
         Message => "Item_Not_Found: Location_Of failed");
      Booch_Status := Item_Not_Found;

   end Location_Of;

end Booch_Light.List_Search;

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
