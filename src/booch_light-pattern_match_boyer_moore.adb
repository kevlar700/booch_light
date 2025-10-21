--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Integer_Utilities;
with Booch_Light.Alogs;

package body Booch_Light.Pattern_Match_Boyer_Moore is

   package Natural_Utilities is new Integer_Utilities (Number => Natural);

   procedure Location_Of
     (The_Pattern  : in     Items;
      In_The_Items : in     Items;
      Result       :    out Index;
      Booch_Status :    out Locus.Location_Of)
   is

      type Skip_Table is array (The_Pattern'Range) of Natural;

      Pattern_Skip  : Skip_Table;
      Pattern_Index : Index;
      Items_Index   : Index :=
        Index'Val (Index'Pos (In_The_Items'First) + The_Pattern'Length - 1);

      function Items_Skip
        (The_Item : in Item)
         return Natural
      is
      begin
         for Temporary_Index in reverse The_Pattern'Range loop
            if The_Item = The_Pattern (Temporary_Index) then
               return (The_Pattern'Length - Index'Pos (Temporary_Index));
            end if;
         end loop;
         return The_Pattern'Length;
      end Items_Skip;

      procedure Preprocess
        (The_Pattern  : in     Items;
         Pattern_Skip : in out Skip_Table)
      is
         Next          : Skip_Table;
         Pattern_Index : Index   := The_Pattern'Last;
         Shift_Amount  : Natural := The_Pattern'Length + 1;
      begin
         for Temporary_Index in The_Pattern'Range loop
            Pattern_Skip (Temporary_Index) :=
              2 * The_Pattern'Length - Index'Pos (Temporary_Index);
         end loop;
         loop
            Next (Pattern_Index) := Shift_Amount;
            while (Shift_Amount <= The_Pattern'Length)
              and then
              (The_Pattern (Pattern_Index) /=
               The_Pattern
                 (Index'Val
                    (Index'Pos (The_Pattern'First) + Shift_Amount - 1)))
            loop
               Pattern_Skip
                 (Index'Val
                    (Index'Pos (The_Pattern'First) + Shift_Amount - 1)) :=
                 Natural_Utilities.Min
                   (Pattern_Skip
                      (Index'Val
                         (Index'Pos (The_Pattern'First) + Shift_Amount - 1)),
                    The_Pattern'Length - Index'Pos (Pattern_Index));
               Shift_Amount                                             :=
                 Next
                   (Index'Val
                      (Index'Pos (The_Pattern'First) + Shift_Amount - 1));
            end loop;
            exit when (Pattern_Index = The_Pattern'First);
            Shift_Amount  := Shift_Amount - 1;
            Pattern_Index := Index'Pred (Pattern_Index);
         end loop;
         for Temporary_Index in
           The_Pattern'First .. Index'Val (Shift_Amount - 1)
         loop
            Pattern_Skip (Temporary_Index) :=
              Natural_Utilities.Min
                (Pattern_Skip (Temporary_Index),
                 The_Pattern'Length + Shift_Amount -
                 Index'Pos (Temporary_Index) - 1);
         end loop;
      end Preprocess;

   begin
      Preprocess (The_Pattern, Pattern_Skip);
      while (Items_Index <= In_The_Items'Last) loop
         Pattern_Index := The_Pattern'Last;
         while In_The_Items (Items_Index) = The_Pattern (Pattern_Index) loop
            if Pattern_Index = The_Pattern'First then
               Result := Items_Index;
               Booch_Status := OK;
               return;
            else
               Pattern_Index := Index'Pred (Pattern_Index);
               Items_Index   := Index'Pred (Items_Index);
            end if;
         end loop;
         Items_Index :=
           Index'Val
             (Index'Pos (Items_Index) +
              Natural_Utilities.Max
                (Items_Skip (In_The_Items (Items_Index)),
                 Pattern_Skip (Pattern_Index)));
      end loop;

      Booch_Status := Pattern_Not_Found;
      Alogs.Log
        (Log_ID  => "5701D42C73911EC3",
         Message => "Pattern_Not_Found: Location_Of failed");

   exception
      when Constraint_Error =>
         Booch_Status := Pattern_Not_Found;
         Alogs.Status_Exception
           (Log_ID  => "E7F195A75987EDBA",
            Message =>
              "Constraint_Error: Pattern_Not_Found: Location_Of failed");
         return;

   end Location_Of;

end Booch_Light.Pattern_Match_Boyer_Moore;

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
