--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.Character_Utilities is

   procedure Make_Uppercase (The_Character : in out Character) is
   begin
      if The_Character in Lowercase_Character then
         The_Character :=
           Character'Val
             (Character'Pos (The_Character) -
              (Character'Pos ('a') - Character'Pos ('A')));
      end if;
   end Make_Uppercase;

   procedure Make_Lowercase (The_Character : in out Character) is
   begin
      if The_Character in Uppercase_Character then
         The_Character :=
           Character'Val
             (Character'Pos (The_Character) +
              (Character'Pos ('a') - Character'Pos ('A')));
      end if;
   end Make_Lowercase;

   function Uppercase
     (The_Character : Character)
      return Character
   is
   begin
      if The_Character in Lowercase_Character then
         return
           Character'Val
             (Character'Pos (The_Character) -
              (Character'Pos ('a') - Character'Pos ('A')));
      else
         return The_Character;
      end if;
   end Uppercase;

   function Lowercase
     (The_Character : Character)
      return Character
   is
   begin
      if The_Character in Uppercase_Character then
         return
           Character'Val
             (Character'Pos (The_Character) +
              (Character'Pos ('a') - Character'Pos ('A')));
      else
         return The_Character;
      end if;
   end Lowercase;

   function Is_Control
     (The_Character : Character)
      return Boolean
   is
   begin
      return (The_Character in Control_Character);
   end Is_Control;

   function Is_Graphic
     (The_Character : Character)
      return Boolean
   is
   begin
      return (The_Character in Graphic_Character);
   end Is_Graphic;

   function Is_Uppercase
     (The_Character : Character)
      return Boolean
   is
   begin
      return (The_Character in Uppercase_Character);
   end Is_Uppercase;

   function Is_Lowercase
     (The_Character : Character)
      return Boolean
   is
   begin
      return (The_Character in Lowercase_Character);
   end Is_Lowercase;

   function Is_Digit
     (The_Character : Character)
      return Boolean
   is
   begin
      return (The_Character in Digit_Character);
   end Is_Digit;

   function Is_Alphabetic
     (The_Character : Character)
      return Boolean
   is
   begin
      return
        ((The_Character in Uppercase_Character)
         or else (The_Character in Lowercase_Character));
   end Is_Alphabetic;

   function Is_Alphanumeric
     (The_Character : Character)
      return Boolean
   is
   begin
      return
        ((The_Character in Uppercase_Character)
         or else (The_Character in Lowercase_Character)
         or else (The_Character in Digit_Character));
   end Is_Alphanumeric;

   function Is_Special
     (The_Character : Character)
      return Boolean
   is
   begin
      return
        ((The_Character in Graphic_Character)
         and then not Is_Alphanumeric (The_Character));
   end Is_Special;

   procedure Value_Of
     (The_Character :     Character;
      Result        :    out Digit;
      Booch_Status  :    out Locus.Value_Of)
   is
   begin
      if The_Character in Digit_Character then
         Result := (Character'Pos (The_Character) - Character'Pos ('0'));
      elsif The_Character in 'A' .. 'F' then
         Result := (Character'Pos (The_Character) - Character'Pos ('A') + 10);
      else
         Result       := Digit'First;
         Booch_Status := Lexical_Error;
         Alogs.Log
           (Log_ID  => "B092967AD2885D4E",
            Message => "Lexical_Error: Value_Of failed");
         return;
      end if;

      Booch_Status := OK;

   end Value_Of;

   function Image_Of
     (The_Digit : Digit)
      return Character
   is
   begin
      if The_Digit < 10 then
         return Character'Val (The_Digit + Character'Pos ('0'));
      else
         return Character'Val (The_Digit + Character'Pos ('A') - 10);
      end if;
   end Image_Of;

   procedure Index_Of
     (The_Character :     Character;
      Result        :    out Letter;
      Booch_Status  :    out Locus.Index_Of)
   is
   begin
      if The_Character in Uppercase_Character then
         Result := (Character'Pos (The_Character) - Character'Pos ('A') + 1);
      elsif The_Character in Lowercase_Character then
         Result := (Character'Pos (The_Character) - Character'Pos ('a') + 1);
      else
         Result       := Letter'Last;
         Booch_Status := Lexical_Error;
         Alogs.Log
           (Log_ID  => "7ADD317BA5D91AEA",
            Message => "Lexical_Error: Index_Of failed");
      end if;

      Booch_Status := OK;

   end Index_Of;

   function Uppercase_Of
     (The_Letter : Letter)
      return Character
   is
   begin
      return Character'Val (The_Letter + Character'Pos ('A') - 1);
   end Uppercase_Of;

   function Lowercase_Of
     (The_Letter : Letter)
      return Character
   is
   begin
      return Character'Val (The_Letter + Character'Pos ('a') - 1);
   end Lowercase_Of;

   function Is_Equal
     (Left           : Character;
      Right          : Character;
      Case_Sensitive : Boolean := True)
      return Boolean
   is
   begin
      if Case_Sensitive then
         return (Left = Right);
      else
         return (Uppercase (Left) = Uppercase (Right));
      end if;
   end Is_Equal;

   function Is_Less_Than
     (Left           : Character;
      Right          : Character;
      Case_Sensitive : Boolean := True)
      return Boolean
   is
   begin
      if Case_Sensitive then
         return (Left < Right);
      else
         return (Uppercase (Left) < Uppercase (Right));
      end if;
   end Is_Less_Than;

   function Is_Greater_Than
     (Left           : Character;
      Right          : Character;
      Case_Sensitive : Boolean := True)
      return Boolean
   is
   begin
      if Case_Sensitive then
         return (Left > Right);
      else
         return (Uppercase (Left) > Uppercase (Right));
      end if;
   end Is_Greater_Than;

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
