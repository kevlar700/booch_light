--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package Booch_Light.String_Utilities is

   package Locus is

      subtype Centered is Status_Code with
          Static_Predicate => Centered in Lexical_Error | OK;

      subtype Left_Justified is Status_Code with
          Static_Predicate => Left_Justified in Lexical_Error | OK;

      subtype Right_Justified is Status_Code with
          Static_Predicate => Right_Justified in Lexical_Error | OK;

   end Locus;

   procedure Make_Uppercase (The_String : in out String);
   procedure Make_Lowercase (The_String : in out String);
   procedure Capitalize (The_String : in out String);
   procedure Uncapitalize (The_String : in out String);

   procedure Replace
     (The_Character      : in     Character;
      With_The_Character : in     Character;
      In_The_String      : in out String;
      Case_Sensitive     : in     Boolean := True);

   function Uppercase
     (The_String : in String)
      return String;

   function Lowercase
     (The_String : in String)
      return String;

   function Capitalized
     (The_String : in String)
      return String;

   function Uncapitalized
     (The_String : in String)
      return String;

   function Replaced
     (The_Character      : in Character;
      With_The_Character : in Character;
      In_The_String      : in String;
      Case_Sensitive     : in Boolean := True)
      return String;

   function Is_Null
     (The_String : in String)
      return Boolean;

   function Is_Control
     (The_String : in String)
      return Boolean;

   function Is_Graphic
     (The_String : in String)
      return Boolean;

   function Is_Uppercase
     (The_String : in String)
      return Boolean;

   function Is_Lowercase
     (The_String : in String)
      return Boolean;

   function Is_Digit
     (The_String : in String)
      return Boolean;

   function Is_Alphabetic
     (The_String : in String)
      return Boolean;

   function Is_Alphanumeric
     (The_String : in String)
      return Boolean;

   function Is_Special
     (The_String : in String)
      return Boolean;

   procedure Centered
     (The_String      : in     String;
      In_The_Width    : in     Positive;
      With_The_Filler : in     Character;
      Centered_String :    out String;
      Booch_Status    :    out Locus.Centered);

   procedure Left_Justified
     (The_String            : in     String;
      In_The_Width          : in     Positive;
      With_The_Filler       : in     Character;
      Left_Justified_String :    out String;
      Booch_Status          :    out Locus.Left_Justified);

   procedure Right_Justified
     (The_String             : in     String;
      In_The_Width           : in     Positive;
      With_The_Filler        : in     Character;
      Right_Justified_String :    out String;
      Booch_Status           :    out Locus.Right_Justified);

   function Stripped
     (The_Character   : in Character;
      From_The_String : in String;
      Case_Sensitive  : in Boolean := True)
      return String;

   function Stripped_Leading
     (The_Character   : in Character;
      From_The_String : in String;
      Case_Sensitive  : in Boolean := True)
      return String;

   function Stripped_Trailing
     (The_Character   : in Character;
      From_The_String : in String;
      Case_Sensitive  : in Boolean := True)
      return String;

   function Number_Of
     (The_Character  : in Character;
      In_The_String  : in String;
      Case_Sensitive : in Boolean := True)
      return Natural;

   function Number_Of
     (The_String     : in String;
      In_The_String  : in String;
      Case_Sensitive : in Boolean := True)
      return Natural;

   function Location_Of
     (The_Character  : in Character;
      In_The_String  : in String;
      Case_Sensitive : in Boolean := True;
      Forward        : in Boolean := True)
      return Natural;

   function Is_Equal
     (Left           : in String;
      Right          : in String;
      Case_Sensitive : in Boolean := True)
      return Boolean;

   function Is_Less_Than
     (Left           : in String;
      Right          : in String;
      Case_Sensitive : in Boolean := True)
      return Boolean;

   function Is_Greater_Than
     (Left           : in String;
      Right          : in String;
      Case_Sensitive : in Boolean := True)
      return Boolean;

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
