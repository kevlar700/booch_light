--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.List_Utilities_Single is

   procedure Location_Of
     (The_Position :     Positive;
      In_The_List  :     List;
      Result       : out List;
      Booch_Status : out Locus.Location_Of)
   is
      Index : List;
   begin
      if Is_Null (In_The_List)
      then
         Alogs.Log
           (Log_ID  => "AC22C41320430FF5",
            Message => "Position_Error: Location_Of failed");
         Booch_Status := Position_Error;
         return;
      else
         Index := In_The_List;
         for Count in 2 .. The_Position loop
            Index := Tail_Of (Index);
            if Is_Null (Index)
            then
               Alogs.Log
                 (Log_ID  => "2E8DB4C90163A999",
                  Message => "Position_Error: Location_Of failed");
               Booch_Status := Position_Error;
               return;
            end if;
         end loop;
      end if;

      Result       := Index;
      Booch_Status := OK;

   end Location_Of;

   procedure Construct
     (The_Items    :        Items;
      And_The_List : in out List)
   is
   begin
      for Index in reverse The_Items'Range loop
         Construct (The_Items (Index), And_The_List);
      end loop;
   end Construct;

   procedure Construct
     (The_List     : in out List;
      And_The_List : in out List)
   is
      Index          : List := Foot_Of (The_List);
      Temporary_List : List := And_The_List;
   begin
      Swap_Tail (Index, Temporary_List);
   end Construct;

   procedure Split
     (The_List        : in out List;
      At_The_Position :        Positive;
      Into_The_List   : in out List;
      Booch_Status    :    out Locus.Split)
   is
      Index      : List;
      Tmp_Status : Locus.Location_Of;
   begin
      Location_Of
        (The_Position => (At_The_Position - 1),
         In_The_List  => The_List,
         Result       => Index,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Position_Error =>
            Booch_Status := Tmp_Status;
            Alogs.Log
              (Log_ID  => "6DAD21766E8350C1",
               Message => "Split: Position_Error failed");
            return;

         when OK =>
            null;
      end case;

      Clear (Into_The_List);
      Swap_Tail (Index, Into_The_List);

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Log
           (Log_ID  => "12CA9E30D200F793",
            Message => "Position_Error: Split failed");
         Booch_Status := Position_Error;
   end Split;

   procedure Insert
     (The_Item           :        Item;
      In_The_List        : in out List;
      After_The_Position :        Positive;
      Booch_Status       :    out Locus.Insert)
   is
      Index          : List;
      Temporary_List : List;
      Tmp_Status     : Locus.Location_Of;
   begin
      Location_Of
        (The_Position => After_The_Position,
         In_The_List  => In_The_List,
         Result       => Index,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Position_Error =>
            Booch_Status := Tmp_Status;
            Alogs.Log
              (Log_ID  => "C26FF4FD0C8E3AC0",
               Message => "Position_Error: Insert failed");
            return;

         when OK =>
            null;
      end case;

      Construct
        (The_Item,
         And_The_List => Temporary_List);
      Swap_Tail
        (Index,
         And_The_List => Temporary_List);
      Index := Tail_Of (Index);
      Swap_Tail
        (Index,
         And_The_List => Temporary_List);

      Booch_Status := OK;

   end Insert;

   procedure Insert
     (The_List           : in out List;
      In_The_List        : in out List;
      After_The_Position :        Positive;
      Booch_Status       :    out Locus.Insert)
   is
      Index          : List;
      Temporary_List : List := The_List;
      Temporary_Tail : List := Foot_Of (The_List);
      Tmp_Status     : Locus.Location_Of;
   begin

      Location_Of
        (The_Position => After_The_Position,
         In_The_List  => In_The_List,
         Result       => Index,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Position_Error =>
            Booch_Status := Tmp_Status;
            Alogs.Log
              (Log_ID  => "454488EE11E85F92",
               Message => "Position_Error: Insert failed");
            return;

         when OK =>
            null;
      end case;

      Swap_Tail
        (Index,
         And_The_List => Temporary_List);
      Swap_Tail
        (Temporary_Tail,
         And_The_List => Temporary_List);

      Booch_Status := OK;

   end Insert;

   procedure Insert
     (The_Item       :        Item;
      After_The_List : in out List)
   is
      Index          : List := Foot_Of (After_The_List);
      Temporary_List : List;
   begin
      Construct
        (The_Item,
         And_The_List => Temporary_List);
      Swap_Tail
        (Index,
         And_The_List => Temporary_List);
   end Insert;

   procedure Insert
     (The_List       :        List;
      After_The_List : in out List)
   is
      Index          : List := Foot_Of (After_The_List);
      Temporary_List : List := The_List;
   begin
      Swap_Tail
        (Index,
         And_The_List => Temporary_List);
   end Insert;

   procedure Remove_Item
     (In_The_List     : in out List;
      At_The_Position :        Positive;
      Booch_Status    :    out Locus.Remove_Item)
   is
      Index          : List;
      Temporary_List : List;
      Tmp_Status     : Locus.Location_Of;

   begin
      Location_Of
        (The_Position => At_The_Position,
         In_The_List  => In_The_List,
         Result       => Index,
         Booch_Status => Tmp_Status);

      case Tmp_Status is
         when Position_Error =>
            Alogs.Log
              (Log_ID  => "F29CE5E4BDE7414E",
               Message => "Position_Error: Remove_Item failed");
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            null;
      end case;

      if Index = In_The_List
      then
         Swap_Tail
           (Index,
            And_The_List => Temporary_List);
         Clear (Index);
         In_The_List := Temporary_List;
      else
         Swap_Tail
           (Index,
            And_The_List => Temporary_List);

         Location_Of
           (The_Position => (At_The_Position - 1),
            In_The_List  => In_The_List,
            Result       => Index,
            Booch_Status => Tmp_Status);

         case Tmp_Status is
            when Position_Error =>
               Alogs.Log
                 (Log_ID  => "8303B81FC0E31469",
                  Message => "Position_Error: Remove_Item failed");
               Booch_Status := Tmp_Status;
               return;

            when OK =>
               null;
         end case;

         Swap_Tail
           (Index,
            And_The_List => Temporary_List);
         Clear (Temporary_List);
      end if;
   end Remove_Item;

   function Foot_Of
     (The_List : List)
      return List
   is
      Index : List := The_List;
   begin
      while not Is_Null (Tail_Of (Index)) loop
         Index := Tail_Of (Index);
      end loop;
      return Index;
   end Foot_Of;

end Booch_Light.List_Utilities_Single;

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
