--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alterable_Log;
with Booch_Light.Character_Utilities;

package body Booch_Light.Integer_Utilities is

   Blank : constant Character := ' ';
   Plus  : constant Character := '+';
   Minus : constant Character := '-';

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
      return (The_Number > 0);
   end Is_Positive;

   function Is_Natural
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number >= 0);
   end Is_Natural;

   function Is_Negative
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number < 0);
   end Is_Negative;

   function Is_Zero
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number = 0);
   end Is_Zero;

   function Is_Odd
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number mod 2 /= 0);
   end Is_Odd;

   function Is_Even
     (The_Number : in Number)
      return Boolean
   is
   begin
      return (The_Number mod 2 = 0);
   end Is_Even;

   function Based_Image
     (The_Number    : in Number;
      With_The_Base : in Base)
      return String
   is
      Result        : Number;
      The_Remainder : Natural;
   begin
      Result        := The_Number / Number (With_The_Base);
      The_Remainder := Natural (The_Number mod Number (With_The_Base));
      if Result = 0 then
         return "" & Character_Utilities.Image_Of (The_Remainder);
      else
         return
           Based_Image (Result, With_The_Base) &
           Character_Utilities.Image_Of (The_Remainder);
      end if;
   end Based_Image;

   function Image_Of
     (The_Number    : in Number;
      With_The_Base : in Base := 10)
      return String
   is
   begin
      if The_Number < 0 then
         return '-' & Based_Image (abs (The_Number), With_The_Base);
      else
         return ' ' & Based_Image (The_Number, With_The_Base);
      end if;
   end Image_Of;

   procedure Value_Of
     (The_Character : in     Character;
      With_The_Base : in     Base;
      Result        :    out Number;
      Booch_Status  :    out Locus.Value_Of)
   is
      Tmp_Number : Character_Utilities.Digit;
      Tmp_Status : Character_Utilities.Locus.Value_Of;
   begin
      Character_Utilities.Value_Of
        (The_Character => The_Character,
         Result        => Tmp_Number,
         Booch_Status  => Tmp_Status);

      case Tmp_Status is
         when Lexical_Error =>
            Alterable_Log.Log
              (Log_ID  => "900C85F5E131FD1A",
               Message => "Lexical_Error: Value_Of failed");
            Result       := Number'Invalid_Value;
            Booch_Status := Tmp_Status;
            return;

         when OK =>
            Result := Number (Tmp_Number);
            null;

      end case;

      if Result > Number (With_The_Base) then
         Alterable_Log.Log
           (Log_ID  => "23D8EF5BE3C01633",
            Message => "Lexical_Error: Value_Of failed");
         Booch_Status := Lexical_Error;
         Result       := Number'Invalid_Value;
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
      Value     : Number  := 0;
      The_Power : Natural := 0;
   begin
      Result := 0;

      for Index in reverse (The_Image'First + 1) .. The_Image'Last loop
         Value_Of
           (The_Character => The_Image (Index),
            With_The_Base => With_The_Base,
            Result        => Value,
            Booch_Status  => Booch_Status);

         case Booch_Status is
            when Lexical_Error =>
               Alterable_Log.Log
                 (Log_ID  => "472F0CBDE7A53CEC",
                  Message => "Lexical_Error: Value_Of failed");
               Result := Number'Invalid_Value;
               return;

            when OK =>
               Value     := Value * (Number (With_The_Base)**The_Power);
               Result    := Result + Value;
               The_Power := The_Power + 1;

         end case;

      end loop;

      if (The_Image (The_Image'First) = Plus)
        or else (The_Image (The_Image'First) = Blank)
      then
         return;

      elsif The_Image (The_Image'First) = Minus then
         Result := -Result;
         return;

      else
         Value_Of
           (The_Character => The_Image (The_Image'First),
            With_The_Base => With_The_Base,
            Result        => Value,
            Booch_Status  => Booch_Status);

         case Booch_Status is
            when Lexical_Error =>
               Alterable_Log.Log
                 (Log_ID  => "0FA76E3385517BCB",
                  Message => "Lexical_Error: Value_Of failed");
               Result := Number'Invalid_Value;

            when OK =>
               Value  := Value * (Number (With_The_Base)**The_Power);
               Result := Result + Value;
         end case;

      end if;

      Booch_Status := OK;

   end Value_Of;

end Booch_Light.Integer_Utilities;

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
