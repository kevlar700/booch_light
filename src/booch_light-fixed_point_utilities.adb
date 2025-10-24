--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;
with Booch_Light.Character_Utilities;
with Booch_Light.Integer_Utilities;
with Booch_Light.String_Utilities;

package body Booch_Light.Fixed_Point_Utilities is

   Blank : constant Character := ' ';
   Plus  : constant Character := '+';
   Minus : constant Character := '-';

   package Utilities is new Integer_Utilities (Number => Integer);

   function Integer_Part
     (The_Number : Number)
      return Integer
   is
   begin
      if Is_Negative (The_Number)
      then
         return Ceiling (The_Number);
      else
         return Floor (The_Number);
      end if;
   end Integer_Part;

   function Real_Part
     (The_Number : Number)
      return Number
   is
   begin
      return abs (The_Number - Number (Integer_Part (The_Number)));
   end Real_Part;

   function Floor
     (The_Number : Number)
      return Integer
   is
      Result : constant Integer := Integer (The_Number);
   begin
      if Number (Result) > The_Number
      then
         return (Result - 1);
      else
         return Result;
      end if;
   end Floor;

   function Ceiling
     (The_Number : Number)
      return Integer
   is
      Result : constant Integer := Integer (The_Number);
   begin
      if Number (Result) < The_Number
      then
         return (Result + 1);
      else
         return Result;
      end if;
   end Ceiling;

   function Min
     (Left  : Number;
      Right : Number)
      return Number
   is
   begin
      if Left > Right
      then
         return Right;
      else
         return Left;
      end if;
   end Min;

   function Min
     (The_Numbers : Numbers)
      return Number
   is
      Smallest_Number : Number := The_Numbers (The_Numbers'First);
   begin
      for Index in (The_Numbers'First + 1) .. The_Numbers'Last loop
         if The_Numbers (Index) < Smallest_Number
         then
            Smallest_Number := The_Numbers (Index);
         end if;
      end loop;
      return Smallest_Number;
   end Min;

   function Max
     (Left  : Number;
      Right : Number)
      return Number
   is
   begin
      if Left > Right
      then
         return Left;
      else
         return Right;
      end if;
   end Max;

   function Max
     (The_Numbers : Numbers)
      return Number
   is
      Largest_Number : Number := The_Numbers (The_Numbers'First);
   begin
      for Index in (The_Numbers'First + 1) .. The_Numbers'Last loop
         if The_Numbers (Index) > Largest_Number
         then
            Largest_Number := The_Numbers (Index);
         end if;
      end loop;
      return Largest_Number;
   end Max;

   function Is_Positive
     (The_Number : Number)
      return Boolean
   is
   begin
      return (The_Number > 0.0);
   end Is_Positive;

   function Is_Natural
     (The_Number : Number)
      return Boolean
   is
   begin
      return (not (The_Number < 0.0));
   end Is_Natural;

   function Is_Negative
     (The_Number : Number)
      return Boolean
   is
   begin
      return (The_Number < 0.0);
   end Is_Negative;

   function Is_Zero
     (The_Number : Number)
      return Boolean
   is
   begin
      return (abs (The_Number) < Number'Small);
   end Is_Zero;

   --  TODO: Replace as recursion is not permitted in this repo
   --  function Real_Image
   --    (The_Fraction  : Number;
   --     With_The_Base : Base;
   --     The_Length    : Natural)
   --     return String
   --  is
   --     Result : constant Number :=
   --       Number (The_Fraction * Number (With_The_Base));
   --  begin
   --     if The_Length = 0
   --     then
   --        return "";
   --     else
   --        return
   --          (Character_Utilities.Image_Of (Integer_Part (Result)) &
   --           Real_Image (Real_Part (Result), With_The_Base, (The_Length - 1)));
   --     end if;
   --  end Real_Image;

   --  TODO: Replace Real_Image as recursion is not permitted in this repo
   --  function Image_Of
   --    (The_Number    : Number;
   --     With_The_Base : Base := 10)
   --     return String
   --  is
   --     The_String : constant String :=
   --       Utilities.Image_Of
   --         (Integer_Part (The_Number), Utilities.Base (With_The_Base)) &
   --       '.' & Real_Image (Real_Part (The_Number), With_The_Base, Number'Aft);
   --  begin
   --     if Is_Negative (The_Number)
   --     then
   --        return Minus & The_String (The_String'First + 1 .. The_String'Last);
   --     else
   --        return The_String;
   --     end if;
   --  end Image_Of;

   function "**"
     (The_Number : Number;
      The_Power  : Integer)
      return Number
   is
      Result : Number := 1.0;
   begin
      for Index in 1 .. abs (The_Power) loop
         Result := Number (Result * The_Number);
      end loop;
      if The_Power < 0
      then
         return Number (Number (1.0) / Result);
      else
         return Result;
      end if;
   end "**";

   procedure Value_Of
     (The_Character :     Character;
      With_The_Base :     Base;
      Result        : out Number;
      Booch_Status  : out Locus.Value_Of)
   is
      Tmp_Status : Character_Utilities.Locus.Value_Of;
      Tmp_Digit  : Character_Utilities.Digit;
   begin
      Character_Utilities.Value_Of
        (The_Character => The_Character,
         Result        => Tmp_Digit,
         Booch_Status  => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alogs.Log
              (Log_ID  => "B00CB334B3C02ECC",
               Message => "Lexical_Error: Fixed point Value_Of failed");
            Booch_Status := Tmp_Status;
            Result       := Number'Last;
            return;

         when OK =>
            Result := Number (Tmp_Digit);

      end case;

      if Result > Number (With_The_Base)
      then
         Alogs.Log
           (Log_ID  => "59EF5270194ABE46",
            Message => "Lexical_Error: Result larger than With_The_Base");
         Booch_Status := Lexical_Error;
         Result       := Number'Last;
         return;
      end if;

      Booch_Status := OK;

   end Value_Of;

   --  TODO: Avoid secondary stack use
   --  procedure Value_Of
   --    (The_Image     :     String;
   --     With_The_Base :     Base := 10;
   --     Result        : out Number;
   --     Booch_Status  : out Locus.Value_Of)
   --  is
   --     Radix_Point       : constant Natural :=
   --       String_Utilities.Location_Of ('.', The_Image);
   --     Tmp_Number        : Number           := 0.0;
   --     The_Power         : Integer;
   --     Predicated_Status : Fixed_Point_Utilities.Locus.Value_Of;
   --  begin
   --     Result := 0.0;
   --
   --     if Radix_Point = 0
   --     then
   --        Alogs.Log
   --          (Log_ID  => "E37B57B38F7F46D7",
   --           Message => "Lexical_Error: Radix_Point was 0");
   --        Booch_Status := Lexical_Error;
   --        Result       := Number'Last;
   --        return;
   --     else
   --        The_Power := Radix_Point - The_Image'Length;
   --        if The_Power = 0
   --        then
   --           Alogs.Log
   --             (Log_ID => "29AECF0EF19E1EFB",
   --
   --              Message => "Lexical_Error: The_Power was 0");
   --           Booch_Status := Lexical_Error;
   --           Result       := Number'Last;
   --           return;
   --        else
   --
   --           for Index in reverse (Radix_Point + 1) .. The_Image'Last loop
   --
   --              Value_Of
   --                (The_Character => The_Image (Index),
   --                 With_The_Base => With_The_Base,
   --                 Result        => Tmp_Number,
   --                 Booch_Status  => Predicated_Status);
   --
   --              case Predicated_Status is
   --                 when Lexical_Error =>
   --                    Alogs.Log
   --                      (Log_ID  => "65DF24B5CE1F3A9F",
   --                       Message =>
   --                         "Lexical_Error: Fixed point Value_Of failed");
   --                    Result := Number'Last;
   --                    return;
   --
   --                 when OK =>
   --                    Result :=
   --                      Result +
   --                      Number
   --                        (Tmp_Number * (Number (With_The_Base)**The_Power));
   --
   --              end case;
   --
   --              The_Power := The_Power + 1;
   --
   --           end loop;
   --
   --           for Index in reverse (The_Image'First + 1) .. (Radix_Point - 1)
   --           loop
   --
   --              Value_Of
   --                (The_Character => The_Image (Index),
   --                 With_The_Base => With_The_Base,
   --                 Result        => Tmp_Number,
   --                 Booch_Status  => Predicated_Status);
   --
   --              case Predicated_Status is
   --                 when Lexical_Error =>
   --                    Alogs.Log
   --                      (Log_ID  => "AD284DE7057244D0",
   --                       Message =>
   --                         "Lexical_Error: Fixed point Value_Of failed");
   --                    Result := Number'Last;
   --                    return;
   --
   --                 when OK =>
   --                    Result :=
   --                      Result +
   --                      Number
   --                        (Tmp_Number * (Number (With_The_Base)**The_Power));
   --
   --              end case;
   --
   --              The_Power := The_Power + 1;
   --
   --           end loop;
   --
   --           if (The_Image (The_Image'First) = Blank)
   --             or else (The_Image (The_Image'First) = Plus)
   --           then
   --              return;
   --           elsif The_Image (The_Image'First) = Minus
   --           then
   --              Result := -Result;
   --              return;
   --           else
   --
   --              Value_Of
   --                (The_Character => The_Image (The_Image'First),
   --                 With_The_Base => With_The_Base,
   --                 Result        => Tmp_Number,
   --                 Booch_Status  => Predicated_Status);
   --
   --              case Predicated_Status is
   --                 when Lexical_Error =>
   --                    Alogs.Log
   --                      (Log_ID  => "BC4814296AEED708",
   --                       Message =>
   --                         "Lexical_Error: Fixed point Value_Of failed");
   --                    Result := Number'Last;
   --                    return;
   --
   --                 when OK =>
   --                    Result :=
   --                      Result +
   --                      Number
   --                        (Tmp_Number * (Number (With_The_Base)**The_Power));
   --
   --              end case;
   --
   --           end if;
   --        end if;
   --     end if;
   --
   --     Booch_Status := OK;
   --  end Value_Of;

   function Is_Equal
     (Left  : Number;
      Right : Number)
      return Boolean
   is
   begin
      return (abs (Left - Right) < Number'Small);
   end Is_Equal;

end Booch_Light.Fixed_Point_Utilities;

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
