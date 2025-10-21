--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.
with Booch_Light.Alogs;

package body Booch_Light.String_Sequential_Bounded_Managed_Iterator is

   procedure Copy
     (From_The_String : in     B_String;
      To_The_String   : in out B_String;
      Booch_Status    :    out Locus.Copy)
   is
   begin
      if From_The_String.The_Length > To_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "75D5925866B65D41",
            Message => "Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;
      end if;

      To_The_String.The_Items (1 .. From_The_String.The_Length) :=
        From_The_String.The_Items (1 .. From_The_String.The_Length);
      To_The_String.The_Length := From_The_String.The_Length;

      Booch_Status := OK;

   end Copy;

   procedure Copy
     (From_The_Substring : in     Substring;
      To_The_String      : in out B_String;
      Booch_Status       :    out Locus.Copy)
   is
   begin
      if From_The_Substring'Length > To_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "7E47195BF0F8D2E5",
            Message => "Exception_Overflow: Copy failed");
         Booch_Status := Exception_Overflow;
         return;
      end if;

      To_The_String.The_Items (1 .. From_The_Substring'Length) :=
        From_The_Substring;
      To_The_String.The_Length := From_The_Substring'Length;

      Booch_Status := OK;

   end Copy;

   procedure Clear (The_String : in out B_String) is
   begin
      The_String.The_Length := 0;
   end Clear;

   procedure Prepend
     (The_String    : in     B_String;
      To_The_String : in out B_String;
      Booch_Status  :    out Locus.Prepend)
   is
      New_Length : constant Natural :=
        To_The_String.The_Length + The_String.The_Length;
   begin

      if New_Length > To_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "389B534B43F2203F",
            Message => "Exception_Overflow: Prepend failed");
         Booch_Status := Exception_Overflow;
         return;
      end if;

      To_The_String.The_Items ((The_String.The_Length + 1) .. New_Length) :=
        To_The_String.The_Items (1 .. To_The_String.The_Length);
      To_The_String.The_Items (1 .. The_String.The_Length)                :=
        The_String.The_Items (1 .. The_String.The_Length);
      To_The_String.The_Length := New_Length;

      Booch_Status := OK;

   end Prepend;

   procedure Prepend
     (The_Substring : in     Substring;
      To_The_String : in out B_String;
      Booch_Status  :    out Locus.Prepend)
   is
      New_Length : constant Natural :=
        To_The_String.The_Length + The_Substring'Length;
   begin

      if New_Length > To_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "B7A9470F4539AFB4",
            Message => "Exception_Overflow: Prepend failed");
         Booch_Status := Exception_Overflow;
         return;
      end if;

      To_The_String.The_Items ((The_Substring'Length + 1) .. New_Length) :=
        To_The_String.The_Items (1 .. To_The_String.The_Length);
      To_The_String.The_Items (1 .. The_Substring'Length) := The_Substring;
      To_The_String.The_Length := New_Length;

      Booch_Status := OK;

   end Prepend;

   procedure Append
     (The_String    : in     B_String;
      To_The_String : in out B_String;
      Booch_Status  :    out Locus.Append)
   is
      New_Length : constant Natural :=
        To_The_String.The_Length + The_String.The_Length;
   begin

      if New_Length > To_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "69E42C6B74CD4407",
            Message => "Exception_Overflow: Append failed");
         Booch_Status := Exception_Overflow;
         return;
      end if;

      To_The_String.The_Items ((To_The_String.The_Length + 1) .. New_Length) :=
        The_String.The_Items (1 .. The_String.The_Length);
      To_The_String.The_Length := New_Length;

      Booch_Status := OK;

   end Append;

   procedure Append
     (The_Substring : in     Substring;
      To_The_String : in out B_String;
      Booch_Status  :    out Locus.Append)
   is
      New_Length : constant Natural :=
        To_The_String.The_Length + The_Substring'Length;
   begin

      if New_Length > To_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "0FEDA7F4307155F3",
            Message => "Exception_Overflow: Append failed");
         Booch_Status := Exception_Overflow;
         return;

      end if;

      To_The_String.The_Items ((To_The_String.The_Length + 1) .. New_Length) :=
        The_Substring;
      To_The_String.The_Length := New_Length;

      Booch_Status := OK;

   end Append;

   procedure Insert
     (The_String      : in     B_String;
      In_The_String   : in out B_String;
      At_The_Position : in     Positive;
      Booch_Status    :    out Locus.Insert)
   is
      New_Length   : constant Natural :=
        In_The_String.The_Length + The_String.The_Length;
      End_Position : constant Natural :=
        At_The_Position + The_String.The_Length;
   begin
      if At_The_Position > In_The_String.The_Length then
         Alogs.Status_Exception
           (Log_ID  => "A2388EC2BBA7F90E",
            Message => "Position_Error: Insert failed");
         Booch_Status := Position_Error;
         return;
      elsif New_Length > In_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "7DC94504E4635959",
            Message => "Exception_Overflow: Insert failed");
         Booch_Status := Exception_Overflow;
         return;

      end if;

      In_The_String.The_Items (End_Position .. New_Length)            :=
        In_The_String.The_Items (At_The_Position .. In_The_String.The_Length);
      In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
        The_String.The_Items (1 .. The_String.The_Length);
      In_The_String.The_Length := New_Length;

      Booch_Status := OK;

   end Insert;

   procedure Insert
     (The_Substring   : in     Substring;
      In_The_String   : in out B_String;
      At_The_Position : in     Positive;
      Booch_Status    :    out Locus.Insert)
   is
      New_Length   : constant Natural :=
        In_The_String.The_Length + The_Substring'Length;
      End_Position : constant Natural :=
        At_The_Position + The_Substring'Length;
   begin
      if At_The_Position > In_The_String.The_Length then
         Alogs.Status_Exception
           (Log_ID  => "5D83E383FF0AE010",
            Message => "Position_Error: Insert failed");
         Booch_Status := Position_Error;
         return;

      elsif New_Length > In_The_String.The_Size then
         Alogs.Status_Exception
           (Log_ID  => "EBB53A430C9E2121",
            Message => "Exception_Overflow: Insert failed");
         Booch_Status := Exception_Overflow;
         return;

      end if;

      In_The_String.The_Items (End_Position .. New_Length)            :=
        In_The_String.The_Items (At_The_Position .. In_The_String.The_Length);
      In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
        The_Substring;
      In_The_String.The_Length := New_Length;

      Booch_Status := OK;

   end Insert;

   procedure Delete
     (In_The_String     : in out B_String;
      From_The_Position : in     Positive;
      To_The_Position   : in     Positive;
      Booch_Status      :    out Locus.Delete)
   is
      New_Length : Natural;
   begin
      if (From_The_Position > In_The_String.The_Length)
        or else (To_The_Position > In_The_String.The_Length)
        or else (From_The_Position > To_The_Position)
      then
         Alogs.Status_Exception
           (Log_ID  => "1DD5C2344C3064AB",
            Message => "Position_Error: Delete failed");
         Booch_Status := Position_Error;
         return;
      end if;

      New_Length                                                :=
        In_The_String.The_Length - (To_The_Position - From_The_Position + 1);
      In_The_String.The_Items (From_The_Position .. New_Length) :=
        In_The_String.The_Items
          ((To_The_Position + 1) .. In_The_String.The_Length);
      In_The_String.The_Length                                  := New_Length;

      Booch_Status := OK;

   end Delete;

   procedure Replace
     (In_The_String   : in out B_String;
      At_The_Position : in     Positive;
      With_The_String : in     B_String;
      Booch_Status    :    out Locus.Replace)
   is
      End_Position : constant Natural :=
        At_The_Position + With_The_String.The_Length - 1;
   begin
      if (At_The_Position > In_The_String.The_Length)
        or else (End_Position > In_The_String.The_Length)
      then
         Alogs.Status_Exception
           (Log_ID  => "8E5BF265658CE669",
            Message => "Position_Error: Replace failed");
         Booch_Status := Position_Error;
         return;
      end if;

      In_The_String.The_Items (At_The_Position .. End_Position) :=
        With_The_String.The_Items (1 .. With_The_String.The_Length);

      Booch_Status := OK;

   end Replace;

   procedure Replace
     (In_The_String      : in out B_String;
      At_The_Position    : in     Positive;
      With_The_Substring : in     Substring;
      Booch_Status       :    out Locus.Replace)
   is
      End_Position : constant Natural :=
        At_The_Position + With_The_Substring'Length - 1;
   begin
      if (At_The_Position > In_The_String.The_Length)
        or else (End_Position > In_The_String.The_Length)
      then
         Alogs.Status_Exception
           (Log_ID  => "7E5D919BFCE2CB98",
            Message => "Position_Error: Replace failed");
         Booch_Status := Position_Error;
         return;
      end if;

      In_The_String.The_Items (At_The_Position .. End_Position) :=
        With_The_Substring;

      Booch_Status := OK;

   end Replace;

   procedure Set_Item
     (In_The_String   : in out B_String;
      At_The_Position : in     Positive;
      With_The_Item   : in     Item;
      Booch_Status    :    out Locus.Set_Item)
   is
   begin
      if At_The_Position > In_The_String.The_Length then
         Alogs.Status_Exception
           (Log_ID  => "40D9030378204C66",
            Message => "Position_Error: Replace failed");
         Booch_Status := Position_Error;
         return;
      end if;

      In_The_String.The_Items (At_The_Position) := With_The_Item;

      Booch_Status := OK;

   end Set_Item;

   function Is_Equal
     (Left  : in B_String;
      Right : in B_String)
      return Boolean
   is
   begin
      if Left.The_Length /= Right.The_Length then
         return False;
      else
         for Index in 1 .. Left.The_Length loop
            if Left.The_Items (Index) /= Right.The_Items (Index) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Is_Equal
     (Left  : in Substring;
      Right : in B_String)
      return Boolean
   is
   begin
      if Left'Length /= Right.The_Length then
         return False;
      else
         for Index in 1 .. Left'Length loop
            if Left (Left'First + Index - 1) /= Right.The_Items (Index) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Is_Equal
     (Left  : in B_String;
      Right : in Substring)
      return Boolean
   is
   begin
      if Left.The_Length /= Right'Length then
         return False;
      else
         for Index in 1 .. Left.The_Length loop
            if Left.The_Items (Index) /= Right (Right'First + Index - 1) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Is_Less_Than
     (Left  : in B_String;
      Right : in B_String)
      return Boolean
   is
   begin
      for Index in 1 .. Left.The_Length loop
         if Index > Right.The_Length then
            return False;
         elsif Left.The_Items (Index) < Right.The_Items (Index) then
            return True;
         elsif Right.The_Items (Index) < Left.The_Items (Index) then
            return False;
         end if;
      end loop;
      return (Left.The_Length < Right.The_Length);
   end Is_Less_Than;

   function Is_Less_Than
     (Left  : in Substring;
      Right : in B_String)
      return Boolean
   is
   begin
      for Index in 1 .. Left'Length loop
         if Index > Right.The_Length then
            return False;
         elsif Left (Left'First + Index - 1) < Right.The_Items (Index) then
            return True;
         elsif Right.The_Items (Index) < Left (Left'First + Index - 1) then
            return False;
         end if;
      end loop;
      return (Left'Length < Right.The_Length);
   end Is_Less_Than;

   function Is_Less_Than
     (Left  : in B_String;
      Right : in Substring)
      return Boolean
   is
   begin
      for Index in 1 .. Left.The_Length loop
         if Index > Right'Length then
            return False;
         elsif Left.The_Items (Index) < Right (Right'First + Index - 1) then
            return True;
         elsif Right (Right'First + Index - 1) < Left.The_Items (Index) then
            return False;
         end if;
      end loop;
      return (Left.The_Length < Right'Length);
   end Is_Less_Than;

   function Is_Greater_Than
     (Left  : in B_String;
      Right : in B_String)
      return Boolean
   is
   begin
      for Index in 1 .. Left.The_Length loop
         if Index > Right.The_Length then
            return True;
         elsif Left.The_Items (Index) < Right.The_Items (Index) then
            return False;
         elsif Right.The_Items (Index) < Left.The_Items (Index) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Greater_Than;

   function Is_Greater_Than
     (Left  : in Substring;
      Right : in B_String)
      return Boolean
   is
   begin
      for Index in 1 .. Left'Length loop
         if Index > Right.The_Length then
            return True;
         elsif Left (Left'First + Index - 1) < Right.The_Items (Index) then
            return False;
         elsif Right.The_Items (Index) < Left (Left'First + Index - 1) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Greater_Than;

   function Is_Greater_Than
     (Left  : in B_String;
      Right : in Substring)
      return Boolean
   is
   begin
      for Index in 1 .. Left.The_Length loop
         if Index > Right'Length then
            return True;
         elsif Left.The_Items (Index) < Right (Right'First + Index - 1) then
            return False;
         elsif Right (Right'First + Index - 1) < Left.The_Items (Index) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Greater_Than;

   function Length_Of
     (The_String : in B_String)
      return Natural
   is
   begin
      return The_String.The_Length;
   end Length_Of;

   function Is_Null
     (The_String : in B_String)
      return Boolean
   is
   begin
      return (The_String.The_Length = 0);
   end Is_Null;

   procedure Item_Of
     (The_String      : in     B_String;
      At_The_Position : in     Positive;
      Result          :    out Item;
      Booch_Status    :    out Locus.Item_Of)
   is
   begin
      if At_The_Position > The_String.The_Length then
         Alogs.Log
           (Log_ID  => "3A0A9ABF65A9B4C4",
            Message => "Position_Error: Item_Of failed");
         Booch_Status := Position_Error;
         return;
      end if;

      Result       := The_String.The_Items (At_The_Position);
      Booch_Status := OK;

   end Item_Of;

   function Substring_Of
     (The_String : in B_String)
      return Substring
   is
   begin
      return The_String.The_Items (1 .. The_String.The_Length);
   end Substring_Of;

   procedure Substring_Of
     (The_String        : in     B_String;
      From_The_Position : in     Positive;
      To_The_Position   : in     Positive;
      Result            :    out Substring;
      Booch_Status      :    out Locus.Substring_Of)
   is
   begin
      if (From_The_Position > The_String.The_Length)
        or else (To_The_Position > The_String.The_Length)
        or else (From_The_Position > To_The_Position)
      then
         Alogs.Log
           (Log_ID  => "80A2DAC7FEFC0394",
            Message => "Position_Error: Substring_Of failed");
         Booch_Status := Position_Error;
         return;
      end if;

      Result := The_String.The_Items (From_The_Position .. To_The_Position);
      Booch_Status := OK;

   end Substring_Of;

   procedure Iterate (Over_The_String : in B_String) is
      Continue : Boolean;
   begin
      for The_Iterator in 1 .. Over_The_String.The_Length loop
         Process (Over_The_String.The_Items (The_Iterator), Continue);
         exit when not Continue;
      end loop;
   end Iterate;

end Booch_Light.String_Sequential_Bounded_Managed_Iterator;

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
