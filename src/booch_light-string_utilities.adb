--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Character_Utilities;
with Booch_Light.Alterable_Log;

package body Booch_Light.String_Utilities is

   procedure Make_Uppercase (The_String : in out String) is
   begin
      for Index in The_String'Range loop
         Character_Utilities.Make_Uppercase (The_String (Index));
      end loop;
   end Make_Uppercase;

   procedure Make_Lowercase (The_String : in out String) is
   begin
      for Index in The_String'Range loop
         Character_Utilities.Make_Lowercase (The_String (Index));
      end loop;
   end Make_Lowercase;

   procedure Capitalize (The_String : in out String) is
   begin
      Character_Utilities.Make_Uppercase (The_String (The_String'First));
   end Capitalize;

   procedure Uncapitalize (The_String : in out String) is
   begin
      Character_Utilities.Make_Lowercase (The_String (The_String'First));
   end Uncapitalize;

   procedure Replace
     (The_Character      : in     Character;
      With_The_Character : in     Character;
      In_The_String      : in out String;
      Case_Sensitive     : in     Boolean := True)
   is
   begin
      for Index in In_The_String'Range loop
         if Character_Utilities.Is_Equal
             (The_Character, In_The_String (Index), Case_Sensitive)
         then
            In_The_String (Index) := With_The_Character;
         end if;
      end loop;
   end Replace;

   function Uppercase
     (The_String : in String)
      return String
   is
      Temporary_String : String (The_String'Range) := The_String;
   begin
      Make_Uppercase (Temporary_String);
      return Temporary_String;
   end Uppercase;

   function Lowercase
     (The_String : in String)
      return String
   is
      Temporary_String : String (The_String'Range) := The_String;
   begin
      Make_Lowercase (Temporary_String);
      return Temporary_String;
   end Lowercase;

   function Capitalized
     (The_String : in String)
      return String
   is
   begin
      return
        (Character_Utilities.Uppercase (The_String (The_String'First)) &
         The_String ((The_String'First + 1) .. The_String'Last));
   end Capitalized;

   function Uncapitalized
     (The_String : in String)
      return String
   is
   begin
      return
        (Character_Utilities.Lowercase (The_String (The_String'First)) &
         The_String ((The_String'First + 1) .. The_String'Last));
   end Uncapitalized;

   function Replaced
     (The_Character      : in Character;
      With_The_Character : in Character;
      In_The_String      : in String;
      Case_Sensitive     : in Boolean := True)
      return String
   is
      Temporary_String : String (In_The_String'Range) := In_The_String;
   begin
      Replace
        (The_Character, With_The_Character, Temporary_String, Case_Sensitive);
      return Temporary_String;
   end Replaced;

   function Is_Null
     (The_String : in String)
      return Boolean
   is
   begin
      return (The_String = "");
   end Is_Null;

   function Is_Control
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Control (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Control;

   function Is_Graphic
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Graphic (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Graphic;

   function Is_Uppercase
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Uppercase (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Uppercase;

   function Is_Lowercase
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Lowercase (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Lowercase;

   function Is_Digit
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Digit (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Digit;

   function Is_Alphabetic
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Alphabetic (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Alphabetic;

   function Is_Alphanumeric
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Alphanumeric (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Alphanumeric;

   function Is_Special
     (The_String : in String)
      return Boolean
   is
   begin
      for Index in The_String'Range loop
         if not Character_Utilities.Is_Special (The_String (Index)) then
            return False;
         end if;
      end loop;
      return (The_String /= "");
   end Is_Special;

   procedure Centered
     (The_String      : in     String;
      In_The_Width    : in     Positive;
      With_The_Filler : in     Character;
      Centered_String :    out String;
      Booch_Status    :    out Locus.Centered)
   is
      Left_Margin  : Natural;
      Right_Margin : Natural;
   begin
      Left_Margin  := (In_The_Width - The_String'Length) / 2;
      Right_Margin := In_The_Width - The_String'Length - Left_Margin;

      Centered_String :=
        (String'(1 .. Left_Margin => With_The_Filler) & The_String &
         String'(1 .. Right_Margin => With_The_Filler));

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "823F10C5BCA80CF2",
            Message => "Lexical_Error: Centering string");
         Booch_Status := Lexical_Error;
         return;

   end Centered;

   procedure Left_Justified
     (The_String            : in     String;
      In_The_Width          : in     Positive;
      With_The_Filler       : in     Character;
      Left_Justified_String :    out String;
      Booch_Status          :    out Locus.Left_Justified)
   is
      Right_Margin : Natural;
   begin
      Right_Margin := In_The_Width - The_String'Length;

      Left_Justified_String :=
        (The_String & String'(1 .. Right_Margin => With_The_Filler));

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "90C9100A54C28001",
            Message => "Lexical_Error: Left justifying string");
         Booch_Status := Lexical_Error;
         return;
   end Left_Justified;

   procedure Right_Justified
     (The_String             : in     String;
      In_The_Width           : in     Positive;
      With_The_Filler        : in     Character;
      Right_Justified_String :    out String;
      Booch_Status           :    out Locus.Right_Justified)
   is
      Left_Margin : Natural;
   begin
      Left_Margin := In_The_Width - The_String'Length;

      Right_Justified_String :=
        (String'(1 .. Left_Margin => With_The_Filler) & The_String);

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alterable_Log.Log
           (Log_ID  => "2B957AFDF054EA20",
            Message => "Lexical_Error: Right justifying string");
         Booch_Status := Lexical_Error;
         return;

   end Right_Justified;

   function Stripped
     (The_Character   : in Character;
      From_The_String : in String;
      Case_Sensitive  : in Boolean := True)
      return String
   is
      Temporary_String : String (From_The_String'Range);
      The_Back         : Natural := Temporary_String'First;
   begin
      for Index in From_The_String'Range loop
         if not Character_Utilities.Is_Equal
             (The_Character, From_The_String (Index), Case_Sensitive)
         then
            Temporary_String (The_Back) := From_The_String (Index);
            The_Back                    := The_Back + 1;
         end if;
      end loop;
      return Temporary_String (Temporary_String'First .. (The_Back - 1));
   end Stripped;

   function Stripped_Leading
     (The_Character   : in Character;
      From_The_String : in String;
      Case_Sensitive  : in Boolean := True)
      return String
   is
      The_Front : Natural := From_The_String'First;
   begin
      for Index in From_The_String'Range loop
         if Character_Utilities.Is_Equal
             (The_Character, From_The_String (Index), Case_Sensitive)
         then
            The_Front := The_Front + 1;
         else
            return From_The_String (The_Front .. From_The_String'Last);
         end if;
      end loop;
      return "";
   end Stripped_Leading;

   function Stripped_Trailing
     (The_Character   : in Character;
      From_The_String : in String;
      Case_Sensitive  : in Boolean := True)
      return String
   is
      The_Back : Natural := From_The_String'Last;
   begin
      for Index in reverse From_The_String'Range loop
         if Character_Utilities.Is_Equal
             (The_Character, From_The_String (Index), Case_Sensitive)
         then
            The_Back := The_Back - 1;
         else
            return From_The_String (From_The_String'First .. The_Back);
         end if;
      end loop;
      return "";
   end Stripped_Trailing;

   function Number_Of
     (The_Character  : in Character;
      In_The_String  : in String;
      Case_Sensitive : in Boolean := True)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Index in In_The_String'Range loop
         if Character_Utilities.Is_Equal
             (The_Character, In_The_String (Index), Case_Sensitive)
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Number_Of;

   function Number_Of
     (The_String     : in String;
      In_The_String  : in String;
      Case_Sensitive : in Boolean := True)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Index in
        In_The_String'First .. (In_The_String'Last - The_String'Length + 1)
      loop
         if Is_Equal
             (The_String,
              In_The_String (Index .. (Index + The_String'Length - 1)),
              Case_Sensitive)
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Number_Of;

   function Location_Of
     (The_Character  : in Character;
      In_The_String  : in String;
      Case_Sensitive : in Boolean := True;
      Forward        : in Boolean := True)
      return Natural
   is
   begin
      if Forward then
         for Index in In_The_String'Range loop
            if Character_Utilities.Is_Equal
                (The_Character, In_The_String (Index), Case_Sensitive)
            then
               return Index;
            end if;
         end loop;
         return 0;
      else
         for Index in reverse In_The_String'Range loop
            if Character_Utilities.Is_Equal
                (The_Character, In_The_String (Index), Case_Sensitive)
            then
               return Index;
            end if;
         end loop;
         return 0;
      end if;
   end Location_Of;

   function Is_Equal
     (Left           : in String;
      Right          : in String;
      Case_Sensitive : in Boolean := True)
      return Boolean
   is
   begin
      if Left'Length /= Right'Length then
         return False;
      else
         for Index in Left'Range loop
            if not Character_Utilities.Is_Equal
                (Left (Index), Right (Right'First + Index - Left'First),
                 Case_Sensitive)
            then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Equal;

   function Is_Less_Than
     (Left           : in String;
      Right          : in String;
      Case_Sensitive : in Boolean := True)
      return Boolean
   is
   begin
      for Index in Left'Range loop
         if Character_Utilities.Is_Less_Than
             (Left (Index), Right (Right'First + Index - Left'First),
              Case_Sensitive)
         then
            return True;
         end if;
      end loop;
      return (Left'Length < Right'Length);
   exception
      when Constraint_Error =>
         return False;
   end Is_Less_Than;

   function Is_Greater_Than
     (Left           : in String;
      Right          : in String;
      Case_Sensitive : in Boolean := True)
      return Boolean
   is
   begin
      for Index in Left'Range loop
         if Character_Utilities.Is_Greater_Than
             (Left (Index), Right (Right'First + Index - Left'First),
              Case_Sensitive)
         then
            return True;
         end if;
      end loop;
      return False;
   exception
      when Constraint_Error =>
         return (Left'Length > Right'Length);
   end Is_Greater_Than;

end Booch_Light.String_Utilities;

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
