--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package Booch_Light.Character_Utilities is

   package Locus is

      subtype Value_Of is Status_Code with
          Static_Predicate => Value_Of in Lexical_Error | OK;

      subtype Index_Of is Status_Code with
          Static_Predicate => Index_Of in Lexical_Error | OK;

   end Locus;

   subtype Control_Character is Character range ASCII.NUL .. ASCII.US;
   subtype Graphic_Character is Character range ' ' .. '~';
   subtype Uppercase_Character is Character range 'A' .. 'Z';
   subtype Lowercase_Character is Character range 'a' .. 'z';
   subtype Digit_Character is Character range '0' .. '9';
   subtype Digit is Integer range 0 .. 15;
   subtype Letter is Integer range 1 .. 26;

   procedure Make_Uppercase (The_Character : in out Character);
   procedure Make_Lowercase (The_Character : in out Character);

   function Uppercase
     (The_Character : Character)
      return Character;

   function Lowercase
     (The_Character : Character)
      return Character;

   function Is_Control
     (The_Character : Character)
      return Boolean;

   function Is_Graphic
     (The_Character : Character)
      return Boolean;

   function Is_Uppercase
     (The_Character : Character)
      return Boolean;

   function Is_Lowercase
     (The_Character : Character)
      return Boolean;

   function Is_Digit
     (The_Character : Character)
      return Boolean;

   function Is_Alphabetic
     (The_Character : Character)
      return Boolean;

   function Is_Alphanumeric
     (The_Character : Character)
      return Boolean;

   function Is_Special
     (The_Character : Character)
      return Boolean;

   procedure Value_Of
     (The_Character :     Character;
      Result        :    out Digit;
      Booch_Status  :    out Locus.Value_Of);

   function Image_Of
     (The_Digit : Digit)
      return Character;

   procedure Index_Of
     (The_Character :     Character;
      Result        :    out Letter;
      Booch_Status  :    out Locus.Index_Of);

   function Uppercase_Of
     (The_Letter : Letter)
      return Character;

   function Lowercase_Of
     (The_Letter : Letter)
      return Character;

   function Is_Equal
     (Left           : Character;
      Right          : Character;
      Case_Sensitive : Boolean := True)
      return Boolean;

   function Is_Less_Than
     (Left           : Character;
      Right          : Character;
      Case_Sensitive : Boolean := True)
      return Boolean;

   function Is_Greater_Than
     (Left           : Character;
      Right          : Character;
      Case_Sensitive : Boolean := True)
      return Boolean;

end Booch_Light.Character_Utilities;

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
