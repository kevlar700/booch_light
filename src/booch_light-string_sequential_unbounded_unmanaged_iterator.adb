--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.
with Booch_Light.Alterable_Log;

package body Booch_Light.String_Sequential_Unbounded_Unmanaged_Iterator is

   procedure Set
     (The_String         : in out U_String;
      To_The_Size        : in     Natural;
      Preserve_The_Value : in     Boolean)
   is
      Temporary_Structure : Structure;
   begin
      if To_The_Size = 0 then
         The_String.The_Items := null;
      elsif The_String.The_Items = null then
         The_String.The_Items := new Substring (1 .. To_The_Size);
      elsif To_The_Size > The_String.The_Items'Length then
         if Preserve_The_Value then
            Temporary_Structure := new Substring (1 .. To_The_Size);
            Temporary_Structure (1 .. The_String.The_Length) :=
              The_String.The_Items (1 .. The_String.The_Length);
            The_String.The_Items := Temporary_Structure;
         else
            The_String.The_Items := new Substring (1 .. To_The_Size);
         end if;
      end if;
      The_String.The_Length := To_The_Size;
   end Set;

   procedure Copy
     (From_The_String : in     U_String;
      To_The_String   : in out U_String;
      Booch_Status    :    out Locus.Copy)
   is
   begin
      Set
        (To_The_String,
         To_The_Size        => From_The_String.The_Length,
         Preserve_The_Value => False);
      To_The_String.The_Items (1 .. From_The_String.The_Length) :=
        From_The_String.The_Items (1 .. From_The_String.The_Length);

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "593C1B3592795C1C",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;
   end Copy;

   procedure Copy
     (From_The_Substring : in     Substring;
      To_The_String      : in out U_String;
      Booch_Status       :    out Locus.Copy)
   is
   begin
      Set
        (To_The_String,
         To_The_Size        => From_The_Substring'Length,
         Preserve_The_Value => False);
      To_The_String.The_Items (1 .. From_The_Substring'Length) :=
        From_The_Substring;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "1B9C8B9A8219B124",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;

   end Copy;

   procedure Clear (The_String : in out U_String) is
   begin
      Set
        (The_String,
         To_The_Size        => 0,
         Preserve_The_Value => False);
   end Clear;

   procedure Prepend
     (The_String    : in     U_String;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Prepend)
   is
      Old_Length : constant Natural := To_The_String.The_Length;
      New_Length : constant Natural :=
        To_The_String.The_Length + The_String.The_Length;
   begin
      Set
        (To_The_String,
         To_The_Size        => New_Length,
         Preserve_The_Value => True);
      To_The_String.The_Items ((The_String.The_Length + 1) .. New_Length) :=
        To_The_String.The_Items (1 .. Old_Length);
      To_The_String.The_Items (1 .. The_String.The_Length)                :=
        The_String.The_Items (1 .. The_String.The_Length);

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "02CE7CEFD818795E",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;

   end Prepend;

   procedure Prepend
     (The_Substring : in     Substring;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Prepend)
   is
      Old_Length : constant Natural := To_The_String.The_Length;
      New_Length : constant Natural :=
        To_The_String.The_Length + The_Substring'Length;
   begin
      Set
        (To_The_String,
         To_The_Size        => New_Length,
         Preserve_The_Value => True);
      To_The_String.The_Items ((The_Substring'Length + 1) .. New_Length) :=
        To_The_String.The_Items (1 .. Old_Length);
      To_The_String.The_Items (1 .. The_Substring'Length) := The_Substring;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "DABABC930B4A0263",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;

   end Prepend;

   procedure Append
     (The_String    : in     U_String;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Append)
   is
      Old_Length : constant Natural := To_The_String.The_Length;
      New_Length : constant Natural :=
        To_The_String.The_Length + The_String.The_Length;
   begin
      Set
        (To_The_String,
         To_The_Size        => New_Length,
         Preserve_The_Value => True);
      To_The_String.The_Items ((Old_Length + 1) .. New_Length) :=
        The_String.The_Items (1 .. The_String.The_Length);

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "45201A51280FB234",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;

   end Append;

   procedure Append
     (The_Substring : in     Substring;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Append)
   is
      Old_Length : constant Natural := To_The_String.The_Length;
      New_Length : constant Natural :=
        To_The_String.The_Length + The_Substring'Length;
   begin
      Set
        (To_The_String,
         To_The_Size        => New_Length,
         Preserve_The_Value => True);
      To_The_String.The_Items ((Old_Length + 1) .. New_Length) :=
        The_Substring;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "A2791142B94CD95A",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;

   end Append;

   procedure Insert
     (The_String      : in     U_String;
      In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      Booch_Status    :    out Locus.Insert)
   is
      Old_Length   : constant Natural := In_The_String.The_Length;
      New_Length   : constant Natural :=
        In_The_String.The_Length + The_String.The_Length;
      End_Position : constant Natural :=
        At_The_Position + The_String.The_Length;
   begin
      if At_The_Position > In_The_String.The_Length then
         Alterable_Log.Status_Exception
           (Log_ID  => "32D209183855D6E8",
            Message => "Position_Error: Insert parameters are invalid");
         Booch_Status := Position_Error;
         return;

      else
         Set
           (In_The_String,
            To_The_Size        => New_Length,
            Preserve_The_Value => True);
         In_The_String.The_Items (End_Position .. New_Length)            :=
           In_The_String.The_Items (At_The_Position .. Old_Length);
         In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
           The_String.The_Items (1 .. The_String.The_Length);
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "038416A27692A719",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;
   end Insert;

   procedure Insert
     (The_Substring   : in     Substring;
      In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      Booch_Status    :    out Locus.Insert)
   is
      Old_Length   : constant Natural := In_The_String.The_Length;
      New_Length   : constant Natural :=
        In_The_String.The_Length + The_Substring'Length;
      End_Position : constant Natural :=
        At_The_Position + The_Substring'Length;
   begin
      if At_The_Position > In_The_String.The_Length then
         Alterable_Log.Status_Exception
           (Log_ID  => "31A5B4830143E690",
            Message => "Position_Error: Insert parameters are invalid");
         Booch_Status := Position_Error;
         return;
      else
         Set
           (In_The_String,
            To_The_Size        => New_Length,
            Preserve_The_Value => True);
         In_The_String.The_Items (End_Position .. New_Length)            :=
           In_The_String.The_Items (At_The_Position .. Old_Length);
         In_The_String.The_Items (At_The_Position .. (End_Position - 1)) :=
           The_Substring;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "5FB3BC4D553F75CE",
            Message => "Storage_Error: Unbounded String");
         Booch_Status := Exception_Storage_Error;
         return;
   end Insert;

   procedure Delete
     (In_The_String     : in out U_String;
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
         Alterable_Log.Status_Exception
           (Log_ID  => "9E76F35759E1FB03",
            Message => "Position_Error: Delete parameters are invalid");
         Booch_Status := Position_Error;
         return;
      else
         New_Length                                                :=
           In_The_String.The_Length -
           (To_The_Position - From_The_Position + 1);
         In_The_String.The_Items (From_The_Position .. New_Length) :=
           In_The_String.The_Items
             ((To_The_Position + 1) .. In_The_String.The_Length);
         Set
           (In_The_String,
            To_The_Size        => New_Length,
            Preserve_The_Value => True);
      end if;

      Booch_Status := OK;
   end Delete;

   procedure Replace
     (In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      With_The_String : in     U_String;
      Booch_Status    :    out Locus.Replace)
   is
      End_Position : constant Natural :=
        At_The_Position + With_The_String.The_Length - 1;
   begin
      if (At_The_Position > In_The_String.The_Length)
        or else (End_Position > In_The_String.The_Length)
      then
         Alterable_Log.Status_Exception
           (Log_ID  => "A14785E8F8E82747",
            Message => "Position_Error: Replace parameters are invalid");
         Booch_Status := Position_Error;
         return;

      else
         In_The_String.The_Items (At_The_Position .. End_Position) :=
           With_The_String.The_Items (1 .. With_The_String.The_Length);
      end if;

      Booch_Status := OK;
   end Replace;

   procedure Replace
     (In_The_String      : in out U_String;
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
         Alterable_Log.Status_Exception
           (Log_ID  => "63D71C86D6DCD9F6",
            Message => "Position_Error: Replace parameters are invalid");
         Booch_Status := Position_Error;
         return;
      else
         In_The_String.The_Items (At_The_Position .. End_Position) :=
           With_The_Substring;
      end if;

      Booch_Status := OK;

   end Replace;

   procedure Set_Item
     (In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      With_The_Item   : in     Item;
      Booch_Status    :    out Locus.Set_Item)
   is
   begin
      if At_The_Position > In_The_String.The_Length then
         Alterable_Log.Status_Exception
           (Log_ID  => "D41C74429AF39CA3",
            Message => "Position_Error: Set_Item parameters are invalid");
         Booch_Status := Position_Error;
         return;
      else
         In_The_String.The_Items (At_The_Position) := With_The_Item;
      end if;

      Booch_Status := OK;
   end Set_Item;

   function Is_Equal
     (Left  : in U_String;
      Right : in U_String)
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
      Right : in U_String)
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
     (Left  : in U_String;
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
     (Left  : in U_String;
      Right : in U_String)
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
      Right : in U_String)
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
     (Left  : in U_String;
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
     (Left  : in U_String;
      Right : in U_String)
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
      Right : in U_String)
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
     (Left  : in U_String;
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
     (The_String : in U_String)
      return Natural
   is
   begin
      return The_String.The_Length;
   end Length_Of;

   function Is_Null
     (The_String : in U_String)
      return Boolean
   is
   begin
      return (The_String.The_Length = 0);
   end Is_Null;

   procedure Item_Of
     (The_String      : in     U_String;
      At_The_Position : in     Positive;
      The_Item        :    out Item;
      Booch_Status    :    out Locus.Item_Of)
   is
   begin
      if At_The_Position > The_String.The_Length then
         Alterable_Log.Status_Exception
           (Log_ID  => "29DF8D5153DD8E16",
            Message => "Position_Error: Item_Of parameters are invalid");
         Booch_Status := Position_Error;
         return;
      else
         The_Item := The_String.The_Items (At_The_Position);
      end if;

      Booch_Status := OK;
   end Item_Of;

   --  function Substring_Of
   --    (The_String : in U_String)
   --     return Substring
   --  is
   --     Temporary_Structure : Substring (1 .. 1);
   --  begin
   --     return The_String.The_Items (1 .. The_String.The_Length);
   --  exception
   --     when Constraint_Error =>
   --        return Temporary_Structure (1 .. 0);
   --  end Substring_Of;

   function Substring_Of_Length
     (The_String : in U_String)
      return Natural
   is
   begin
      return The_String.The_Length;
   end Substring_Of_Length;

   procedure Substring_Of
     (The_String    : in     U_String;
      The_Substring : in out Substring;
      Booch_Status  :    out Locus.Substring_Of)
   is
      Temporary_Structure : Substring (1 .. 1);
   begin

      The_Substring := The_String.The_Items (1 .. The_String.The_Length);

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "11975F007D5FFBD8",
            Message =>
              "Constraint_Error: The_Substring is of an incorrect size");
         Booch_Status  := Exception_Constraint_Error;
         The_Substring := Temporary_Structure (1 .. 0);

   end Substring_Of;

   function Substring_Of_Length
     (The_String        : in U_String;
      From_The_Position : in Positive;
      To_The_Position   : in Positive)
      return Natural
   is
   begin
      return
        The_String.The_Items (From_The_Position .. To_The_Position)'Length;
   end Substring_Of_Length;

   procedure Substring_Of
     (The_String        : in     U_String;
      From_The_Position : in     Positive;
      To_The_Position   : in     Positive;
      The_Substring     : in out Substring;
      Booch_Status      :    out Locus.Substring_Of)
   is
      Temporary_Structure : Substring (1 .. 1);
   begin
      if (From_The_Position > The_String.The_Length)
        or else (To_The_Position > The_String.The_Length)
        or else (From_The_Position > To_The_Position)
      then
         Alterable_Log.Log
           (Log_ID  => "B94330FCA1E409AB",
            Message => "Invalid Substring requested");
         Booch_Status := Position_Error;
         return;
      else
         The_Substring :=
           The_String.The_Items (From_The_Position .. To_The_Position);

      end if;

   exception
      when Constraint_Error =>
         Alterable_Log.Status_Exception
           (Log_ID  => "0E69273FD345B148",
            Message =>
              "Constraint_Error: The_Substring is of an incorrect size");
         Booch_Status  := Exception_Constraint_Error;
         The_Substring := Temporary_Structure (1 .. 0);
   end Substring_Of;

   procedure Iterate (Over_The_String : in U_String) is
      Continue : Boolean;
   begin
      for The_Iterator in 1 .. Over_The_String.The_Length loop
         Process (Over_The_String.The_Items (The_Iterator), Continue);
         exit when not Continue;
      end loop;
   end Iterate;

   --  function Substring_Of
   --    (The_String        : in U_String;
   --     From_The_Position : in Positive;
   --     To_The_Position   : in Positive)
   --     return Substring
   --  is
   --  begin
   --     if (From_The_Position > The_String.The_Length)
   --       or else (To_The_Position > The_String.The_Length)
   --       or else (From_The_Position > To_The_Position)
   --     then
   --        raise Position_Error;
   --     else
   --        return The_String.The_Items
   --                (From_The_Position .. To_The_Position);
   --     end if;
   --  end Substring_Of;

end Booch_Light.String_Sequential_Unbounded_Unmanaged_Iterator;

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
