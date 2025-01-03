--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Number is digits <>;
package Booch_Light.Floating_Point_Utilities is

   package Locus is

      subtype Value_Of is Status_Code with
          Static_Predicate => Value_Of in Lexical_Error | OK;

   end Locus;

   type Base is range 2 .. 16;

   type Numbers is array (Positive range <>) of Number;

   function Integer_Part
     (The_Number : in Number)
      return Integer;

   function Real_Part
     (The_Number : in Number)
      return Number;

   function Floor
     (The_Number : in Number)
      return Integer;

   function Ceiling
     (The_Number : in Number)
      return Integer;

   function Min
     (Left  : in Number;
      Right : in Number)
      return Number;

   function Min
     (The_Numbers : in Numbers)
      return Number;

   function Max
     (Left  : in Number;
      Right : in Number)
      return Number;

   function Max
     (The_Numbers : in Numbers)
      return Number;

   function Is_Positive
     (The_Number : in Number)
      return Boolean;

   function Is_Natural
     (The_Number : in Number)
      return Boolean;

   function Is_Negative
     (The_Number : in Number)
      return Boolean;

   function Is_Zero
     (The_Number : in Number)
      return Boolean;

   function Image_Of
     (The_Number    : in Number;
      With_The_Base : in Base := 10)
      return String;

   procedure Value_Of
     (The_Image     : in     String;
      With_The_Base : in     Base := 10;
      Result        :    out Number;
      Booch_Status  :    out Locus.Value_Of);

   function Is_Equal
     (Left  : in Number;
      Right : in Number)
      return Boolean;

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
