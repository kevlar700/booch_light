--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Storage_Manager_Sequential is

   Free_List : Pointer := null;

   procedure Free (The_Pointer : in out Pointer) is
      Temporary_Pointer : Pointer;
   begin
      while The_Pointer /= null loop
         Temporary_Pointer := The_Pointer;
         The_Pointer       := Pointer_Of (The_Pointer.all);
         Free (Temporary_Pointer.all);
         Set_Pointer
           (Temporary_Pointer.all,
            The_Pointer => Free_List);
         Free_List := Temporary_Pointer;
      end loop;
   end Free;

   function New_Item return Pointer is
      Temporary_Pointer : Pointer;
   begin
      if Free_List = null
      then
         return new Item;
      else
         Temporary_Pointer := Free_List;
         Free_List         := Pointer_Of (Temporary_Pointer.all);
         Set_Pointer
           (Temporary_Pointer.all,
            The_Pointer => null);
         return Temporary_Pointer;
      end if;
   end New_Item;

end Booch_Light.Storage_Manager_Sequential;

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
