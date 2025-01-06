--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;

package body Booch_Light.Stack_Sequential_Bounded_Managed_Noniterator is

   procedure Copy
     (From_The_Stack : in     Stack;
      To_The_Stack   : in out Stack;
      Booch_Status   :    out Locus.Copy)
   is
   begin
      if From_The_Stack.The_Top > To_The_Stack.The_Size then
         Alterable_Log.Status_Exception
           (Log_ID => "C6368FE9EC9FBE11",

            Message => "Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;
      else
         To_The_Stack.The_Items (1 .. From_The_Stack.The_Top) :=
           From_The_Stack.The_Items (1 .. From_The_Stack.The_Top);
         To_The_Stack.The_Top := From_The_Stack.The_Top;
      end if;
   end Copy;

   procedure Clear (The_Stack : in out Stack) is
   begin
      The_Stack.The_Top := 0;
   end Clear;

   procedure Push
     (The_Item     : in     Item;
      On_The_Stack : in out Stack;
      Booch_Status :    out Locus.Push)
   is
   begin
      On_The_Stack.The_Items (On_The_Stack.The_Top + 1) := The_Item;
      On_The_Stack.The_Top := On_The_Stack.The_Top + 1;

   exception
      when Constraint_Error =>
         Booch_Status := Exception_Overflow;
         Alterable_Log.Status_Exception
           (Log_ID  => "F2AC932489A5DFEB",
            Message => "Constraint_Error: Push failed");
         return;
   end Push;

   procedure Pop
     (The_Stack    : in out Stack;
      Booch_Status :    out Locus.Pop)
   is
   begin
      The_Stack.The_Top := The_Stack.The_Top - 1;

   exception
      when Constraint_Error =>
         Booch_Status := Exception_Underflow;
         Alterable_Log.Status_Exception
           (Log_ID  => "90424FAB264A5700",
            Message => "Constraint_Error: Pop failed");
         return;
   end Pop;

   function Is_Equal
     (Left  : in Stack;
      Right : in Stack)
      return Boolean
   is
   begin
      if Left.The_Top /= Right.The_Top then
         return False;
      else
         for Index in 1 .. Left.The_Top loop
            if Left.The_Items (Index) /= Right.The_Items (Index) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Depth_Of
     (The_Stack : in Stack)
      return Natural
   is
   begin
      return The_Stack.The_Top;
   end Depth_Of;

   function Is_Empty
     (The_Stack : in Stack)
      return Boolean
   is
   begin
      return (The_Stack.The_Top = 0);
   end Is_Empty;

   procedure Top_Of
     (The_Stack    : in     Stack;
      The_Top      :    out Item;
      Booch_Status :    out Locus.Top_Of)
   is
   begin
      The_Top := The_Stack.The_Items (The_Stack.The_Top);

   exception
      when Constraint_Error =>
         Booch_Status := Exception_Underflow;
         Alterable_Log.Status_Exception
           (Log_ID  => "B6CE3EF111B8DE92",
            Message => "Constraint_Error: Top_Of failed");
         return;
   end Top_Of;

end Booch_Light.Stack_Sequential_Bounded_Managed_Noniterator;

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
