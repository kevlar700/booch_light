--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Ordered_Sequential_Search is

   procedure Location_Of
     (The_Key      : in     Key;
      In_The_Items : in     Items;
      Result       :    out Index;
      Booch_Status :    out Locus.Location_Of)
   is
   begin
      for The_Index in In_The_Items'Range loop
         if Is_Equal (The_Key, In_The_Items (The_Index)) then
            Result       := The_Index;
            Booch_Status := OK;
            return;
         elsif Is_Less_Than (The_Key, In_The_Items (The_Index)) then
            Booch_Status := Item_Not_Found;
         end if;
      end loop;

      Booch_Status := Item_Not_Found;

   end Location_Of;

end Booch_Light.Ordered_Sequential_Search;

--              Original Booch Components (Ada 83 version)
--  License: MIT
--  Copyright (C) 1987 Grady Booch Copyright (C) 2024 Kevin Chadwick (Light
--  runtime compatibility)- DEALINGS IN THE SOFTWARE.
