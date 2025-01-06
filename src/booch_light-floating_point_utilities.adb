--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;
with Booch_Light.Character_Utilities;
with Booch_Light.String_Utilities;
with Booch_Light.Integer_Utilities;

package body Booch_Light.Floating_Point_Utilities is

   Blank : constant Character := ' ';
   Plus  : constant Character := '+';
   Minus : constant Character := '-';

   package Utilities is new Integer_Utilities (Number => Integer);

   function Integer_Part
     (The_Number : in Number)
      return Integer
   is
   begin
      if Is_Negative (The_Number) then
         return Ceiling (The_Number);
      else
         return Floor (The_Number);
      end if;
   end Integer_Part;

   function Real_Part
     (The_Number : in Number)
      return Number
   is
   begin
      return abs (The_Number - Number (Integer_Part (The_Number)));
   end Real_Part;

   function Floor
     (The_Number : in Number)
      return Integer
   is
      Result : constant Integer := Integer (The_Number);
   begin
      if Number (Result) > The_Number then
         return (Result - 1);
      else
         return Result;
      end if;
   end Floor;

   function Ceiling
     (The_Number : in Number)
      return Integer
   is
      Result : constant Integer := Integer (The_Number);
   begin
      if Number (Result) < The_Number then
         return (Result + 1);
      else
         return Result;
      end if;
   end Ceiling;

   function Min
     (Left  : in Number;
      Right : in Number)
      return Number
   is
   begin
      if Left > Right then
         return Right;
      else
         return Left;
      end if;
   end Min;

   function Min
     (The_Numbers : in Numbers)
      return Number
   is
      Smallest_Number : Number := The_Numbers (The_Numbers'First);
   begin
      for Index in (The_Numbers'First + 1) .. The_Numbers'Last loop
         if The_Numbers (Index) < Smallest_Number then
            Smallest_Number := The_Numbers (Index);
         end if;
      end loop;
      return Smallest_Number;
   end Min;

   function Max
     (Left  : in Number;
      Right : in Number)
      return Number
   is
   begin
      if Left > Right then
         return Left;
      else
         return Right;
      end if;
   end Max;

   function Max
     (The_Numbers : in Numbers)
      return Number
   is
      Largest_Number : Number := The_Numbers (The_Numbers'First);
   begin
      for Index in (The_Numbers'First + 1) .. The_Numbers'Last loop
         if The_Numbers (Index) > Largest_Number then
            Largest_Number := The_Numbers (Index);
         end if;
      end loop;
      return Largest_Number;
   end Max;

   function Is_Positive
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number > 0.0);
   end Is_Positive;

   function Is_Natural
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (not (The_Number < 0.0));
   end Is_Natural;

   function Is_Negative
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number < 0.0);
   end Is_Negative;

   function Is_Zero
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (abs (The_Number) < Number'Small);
   end Is_Zero;

   function Real_Image
     (The_Fraction  : in Number;
      With_The_Base : in Base;
      The_Length    : in Natural)
      return String
   is
      The_Value : constant Number := The_Fraction * Number (With_The_Base);
   begin
      if The_Length = 0 then
         return "";
      else
         return
           (Character_Utilities.Image_Of (Integer_Part (The_Value)) &
            Real_Image
              (Real_Part (The_Value), With_The_Base, (The_Length - 1)));
      end if;
   end Real_Image;

   function Based_Image
     (The_Image     : in String;
      The_Fraction  : in Number;
      With_The_Base : in Base)
      return String
   is
      Significant_Digits : constant Integer :=
        (Number'Digits - The_Image'Length + 1);
   begin
      if Significant_Digits > 0 then
         return
           The_Image & '.' &
           Real_Image (The_Fraction, With_The_Base, Significant_Digits);
      else
         return The_Image & ".0";
      end if;
   end Based_Image;

   function Image_Of
     (The_Number    : in Number;
      With_The_Base : in Base := 10)
      return String
   is
      The_String : constant String :=
        Based_Image
          (Utilities.Image_Of
             (Integer_Part (The_Number), Utilities.Base (With_The_Base)),
           Real_Part (The_Number), With_The_Base);
   begin
      if Is_Negative (The_Number) then
         return Minus & The_String (The_String'First + 1 .. The_String'Last);
      else
         return The_String;
      end if;
   end Image_Of;

   procedure Value_Of
     (The_Character : in     Character;
      With_The_Base : in     Base;
      Result        :    out Number;
      Booch_Status  :    out Locus.Value_Of)
   is
      Tmp_Result : Character_Utilities.Digit;
      Tmp_Status : Character_Utilities.Locus.Value_Of;
   begin
      Character_Utilities.Value_Of
        (The_Character => The_Character,
         Result        => Tmp_Result,
         Booch_Status  => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Booch_Status := Tmp_Status;
            Result       := Number'Invalid_Value;
            Alterable_Log.Log
              (Log_ID  => "B7040B7522D02BC5",
               Message => "Lexical_Error: Value_Of failed");
            return;

         when OK =>
            Result := Number (Tmp_Result);
      end case;

      if Result > Number (With_The_Base) then
         Booch_Status := Lexical_Error;
         Result       := Number'Invalid_Value;
         Alterable_Log.Log
           (Log_ID  => "850DD8955C4CB08D",
            Message => "Lexical_Error: Value_Of failed");
         return;
      end if;

      Booch_Status := OK;

   end Value_Of;

   procedure Value_Of
     (The_Image     : in     String;
      With_The_Base : in     Base := 10;
      Result        :    out Number;
      Booch_Status  :    out Locus.Value_Of)
   is
      Radix_Point : constant Natural :=
        String_Utilities.Location_Of ('.', The_Image);
      Tmp_Result  : Number           := 0.0;
      The_Power   : Integer;
   begin
      Result := 0.0;

      if Radix_Point = 0 then
         Booch_Status := Lexical_Error;
         Result       := Number'Invalid_Value;
         Alterable_Log.Log
           (Log_ID  => "E616847CF2F52C4D",
            Message => "Lexical_Error: Value_Of failed");
         return;
      else
         The_Power := Radix_Point - The_Image'Length;
         if The_Power = 0 then
            Booch_Status := Lexical_Error;
            Result       := Number'Invalid_Value;
            Alterable_Log.Log
              (Log_ID  => "2186D3A3A0FFEFB4",
               Message => "Lexical_Error: Value_Of failed");
            return;
         else
            for Index in reverse (Radix_Point + 1) .. The_Image'Last loop
               Value_Of
                 (The_Character => The_Image (Index),
                  With_The_Base => With_The_Base,
                  Result        => Tmp_Result,
                  Booch_Status  => Booch_Status);

               Result :=
                 Result + Tmp_Result * (Number (With_The_Base)**The_Power);

               case Booch_Status is
                  when Lexical_Error =>
                     Result := Number'Invalid_Value;
                     Alterable_Log.Log
                       (Log_ID  => "BCBF5C05B8C41092",
                        Message => "Lexical_Error: Value_Of failed");
                     return;

                  when OK =>
                     null;

               end case;

               The_Power := The_Power + 1;
            end loop;

            for Index in reverse (The_Image'First + 1) .. (Radix_Point - 1)
            loop

               Value_Of
                 (The_Character => The_Image (Index),
                  With_The_Base => With_The_Base,
                  Result        => Tmp_Result,
                  Booch_Status  => Booch_Status);

               Result :=
                 Result + Tmp_Result * (Number (With_The_Base)**The_Power);

               case Booch_Status is
                  when Lexical_Error =>
                     Result := Number'Invalid_Value;
                     Alterable_Log.Log
                       (Log_ID  => "592A37B4387C83D3",
                        Message => "Lexical_Error: Value_Of failed");
                     return;

                  when OK =>
                     null;

               end case;

               The_Power := The_Power + 1;

            end loop;

            if (The_Image (The_Image'First) = Blank)
              or else (The_Image (The_Image'First) = Plus)
            then
               return;
            elsif The_Image (The_Image'First) = Minus then
               Result := -Result;
               return;
            else

               Value_Of
                 (The_Character => The_Image (The_Image'First),
                  With_The_Base => With_The_Base,
                  Result        => Tmp_Result,
                  Booch_Status  => Booch_Status);

               Result :=
                 Result + Tmp_Result * (Number (With_The_Base)**The_Power);

               case Booch_Status is
                  when Lexical_Error =>
                     Result := Number'Invalid_Value;
                     Alterable_Log.Log
                       (Log_ID  => "73B4C549B7259974",
                        Message => "Lexical_Error: Value_Of failed");
                     return;

                  when OK =>
                     return;

               end case;

            end if;
         end if;
      end if;
   end Value_Of;

   function Is_Equal
     (Left  : in Number;
      Right : in Number)
      return Boolean
   is
   begin
      return (abs (Left - Right) < Number'Small);
   end Is_Equal;

end Booch_Light.Floating_Point_Utilities;

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
