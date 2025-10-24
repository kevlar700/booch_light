--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light
  .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator is

   procedure Copy
     (From_The_Queue :     Queue;
      To_The_Queue   : in out Queue;
      Booch_Status   :    out Locus.Copy)
   is
   begin
      if From_The_Queue.The_Back > To_The_Queue.The_Size then
         Booch_Status := Exception_Overflow;
         Alogs.Log
           (Log_ID  => "80ADF5233BE70396",
            Message => "Exception_Overflow: Copy failed");
         return;
      elsif From_The_Queue.The_Back = 0 then
         To_The_Queue.The_Back := 0;
      else
         To_The_Queue.The_Items (1 .. From_The_Queue.The_Back) :=
           From_The_Queue.The_Items (1 .. From_The_Queue.The_Back);
         To_The_Queue.The_Back := From_The_Queue.The_Back;
      end if;

      Booch_Status := OK;
   end Copy;

   procedure Clear (The_Queue : in out Queue) is
   begin
      The_Queue.The_Back := 0;
   end Clear;

   procedure Add
     (The_Item     :     Item;
      To_The_Queue : in out Queue;
      Booch_Status :    out Locus.Add)
   is
   begin
      To_The_Queue.The_Items (To_The_Queue.The_Back + 1) := The_Item;
      To_The_Queue.The_Back := To_The_Queue.The_Back + 1;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := Exception_Overflow;
         Alogs.Log
           (Log_ID  => "A2B9AD5091648740",
            Message => "Exception_Overflow: Add failed");

         return;
   end Add;

   procedure Pop
     (The_Queue    : in out Queue;
      Booch_Status :    out Locus.Pop)
   is
   begin
      if The_Queue.The_Back = 0 then
         Booch_Status := Exception_Underflow;
         Alogs.Log
           (Log_ID  => "B60BBB0DA67DBCB3",
            Message => "Exception_Underflow: Pop failed");
         return;
      elsif The_Queue.The_Back = 1 then
         The_Queue.The_Back := 0;
      else
         The_Queue.The_Items (1 .. (The_Queue.The_Back - 1)) :=
           The_Queue.The_Items (2 .. The_Queue.The_Back);
         The_Queue.The_Back := The_Queue.The_Back - 1;
      end if;

      Booch_Status := OK;
   end Pop;

   function Is_Equal
     (Left  : Queue;
      Right : Queue)
      return Boolean
   is
   begin
      if Left.The_Back /= Right.The_Back then
         return False;
      else
         for Index in 1 .. Left.The_Back loop
            if Left.The_Items (Index) /= Right.The_Items (Index) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Length_Of
     (The_Queue : Queue)
      return Natural
   is
   begin
      return The_Queue.The_Back;
   end Length_Of;

   function Is_Empty
     (The_Queue : Queue)
      return Boolean
   is
   begin
      return (The_Queue.The_Back = 0);
   end Is_Empty;

   procedure Front_Of
     (The_Queue    :     Queue;
      The_Front    :    out Item;
      Booch_Status :    out Locus.Front_Of)
   is
   begin
      if The_Queue.The_Back = 0 then
         Booch_Status := Exception_Underflow;
         Alogs.Log
           (Log_ID  => "4541171554C78473",
            Message => "Exception_Overflow: Front_Of failed");
         return;
      else
         The_Front := The_Queue.The_Items (1);
      end if;

      Booch_Status := OK;
   end Front_Of;

end Booch_Light
  .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator;

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
