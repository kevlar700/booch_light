--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Input_Filter is

   Lookahead : array (1 .. 2) of Item;
   Count     : Natural := 0;

   procedure Clear is
   begin
      Count := 0;
   end Clear;

   procedure Input (The_Item : out Item) is
   begin
      if Count = 0 then
         if Is_End_Of_Line then
            The_Item := Line_Terminator;
            if Is_End_Of_Page then
               Lookahead (1) := Page_Terminator;
               Count         := 1;
               if Is_End_Of_File then
                  Lookahead (2) := File_Terminator;
                  Count         := 2;
               else
                  Skip_Page;
               end if;
            else
               Skip_Line;
            end if;
         else
            Get (The_Item);
         end if;
      else
         The_Item := Lookahead (1);
         if Lookahead (1) /= File_Terminator then
            Lookahead (1) := Lookahead (2);
            Count         := Count - 1;
         end if;
      end if;
   end Input;

end Booch_Light.Input_Filter;

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
