--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Pattern_Match_Knuth_Morris_Pratt is

   procedure Location_Of
     (The_Pattern  : in     Items;
      In_The_Items : in     Items;
      Result       :    out Index;
      Booch_Status :    out Locus.Location_Of)
   is

      type Skip_Table is array (The_Pattern'Range) of Natural;

      Skip_All      : Boolean := False;
      Pattern_Skip  : Skip_Table;
      Pattern_Index : Index   := The_Pattern'First;
      Items_Index   : Index   := In_The_Items'First;

      procedure Preprocess
        (The_Pattern  : in     Items;
         Pattern_Skip : in out Skip_Table)
      is
         Pattern_Index : Index   := The_Pattern'First;
         Shift_Amount  : Natural := 0;
      begin
         Pattern_Skip (Pattern_Skip'First) := 0;
         while Pattern_Index < Pattern_Skip'Last loop
            while (Shift_Amount > 0)
              and then
              (The_Pattern
                 (Index'Val
                    (Index'Pos (The_Pattern'First) + Shift_Amount - 1)) /=
               The_Pattern (Pattern_Index))
            loop
               Shift_Amount :=
                 Pattern_Skip
                   (Index'Val
                      (Index'Pos (Pattern_Skip'First) + Shift_Amount - 1));
            end loop;
            Shift_Amount  := Shift_Amount + 1;
            Pattern_Index := Index'Succ (Pattern_Index);
            if The_Pattern
                (Index'Val
                   (Index'Pos (The_Pattern'First) + Shift_Amount - 1)) =
              The_Pattern (Pattern_Index)
            then
               Pattern_Skip (Pattern_Index) :=
                 Pattern_Skip
                   (Index'Val
                      (Index'Pos (Pattern_Skip'First) + Shift_Amount - 1));
            else
               Pattern_Skip (Pattern_Index) := Shift_Amount;
            end if;
         end loop;
      end Preprocess;

   begin
      Preprocess (The_Pattern, Pattern_Skip);
      while
        ((Pattern_Index <= The_Pattern'Last)
         and then (Items_Index <= In_The_Items'Last))
      loop
         while In_The_Items (Items_Index) /= The_Pattern (Pattern_Index) loop
            if Pattern_Skip (Pattern_Index) = 0 then
               Skip_All := True;
               exit;
            else
               Pattern_Index :=
                 Index'Val
                   (Index'Pos (The_Pattern'First) +
                    Pattern_Skip (Pattern_Index) - 1);
            end if;
         end loop;
         if (Pattern_Index = The_Pattern'Last) and then not Skip_All then
            Result       :=
              Index'Val (Index'Pos (Items_Index) - The_Pattern'Length + 1);
            Booch_Status := OK;
            return;
         else
            if Skip_All then
               Pattern_Index := The_Pattern'First;
               Skip_All      := False;
            else
               Pattern_Index := Index'Succ (Pattern_Index);
            end if;
            Items_Index := Index'Succ (Items_Index);
         end if;
      end loop;

      Booch_Status := Pattern_Not_Found;
      Alterable_Log.Log
        (Log_ID  => "AE4CE426DD6CEE8B",
         Message => "Pattern_Not_Found: Location_Of failed");

   exception
      when Constraint_Error =>
         Booch_Status := Pattern_Not_Found;
         Alterable_Log.Status_Exception
           (Log_ID  => "68711A6F72CCC1E5",
            Message =>
              "Constraint_Error: Pattern_Not_Found: Location_Of failed");
         return;

   end Location_Of;

end Booch_Light.Pattern_Match_Knuth_Morris_Pratt;

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
