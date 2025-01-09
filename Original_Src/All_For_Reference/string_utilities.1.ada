--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

package String_Utilities is

    procedure Make_Uppercase   (The_String         : in out String);
    procedure Make_Lowercase   (The_String         : in out String);
    procedure Capitalize       (The_String         : in out String);
    procedure Uncapitalize     (The_String         : in out String);
    procedure Replace          (The_Character      : in     Character; 
                                With_The_Character : in     Character; 
                                In_The_String      : in out String; 
                                Case_Sensitive     : in     Boolean := True);

    function Uppercase         (The_String         : in String) return String;
    function Lowercase         (The_String         : in String) return String;
    function Capitalized       (The_String         : in String) return String;
    function Uncapitalized     (The_String         : in String) return String;
    function Replaced          (The_Character      : in Character; 
                                With_The_Character : in Character; 
                                In_The_String      : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return String;

    function Is_Null           (The_String         : in String) return Boolean;
    function Is_Control        (The_String         : in String) return Boolean;
    function Is_Graphic        (The_String         : in String) return Boolean;
    function Is_Uppercase      (The_String         : in String) return Boolean;
    function Is_Lowercase      (The_String         : in String) return Boolean;
    function Is_Digit          (The_String         : in String) return Boolean;
    function Is_Alphabetic     (The_String         : in String) return Boolean;
    function Is_Alphanumeric   (The_String         : in String) return Boolean;
    function Is_Special        (The_String         : in String) return Boolean;

    function Centered          (The_String         : in String;  
                                In_The_Width       : in Positive; 
                                With_The_Filler    : in Character)
                                                                return String;
    function Left_Justified    (The_String         : in String; 
                                In_The_Width       : in Positive; 
                                With_The_Filler    : in Character)
                                                                return String;
    function Right_Justified   (The_String         : in String; 
                                In_The_Width       : in Positive; 
                                With_The_Filler    : in Character)       
                                                                return String;
    function Stripped          (The_Character      : in Character; 
                                From_The_String    : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return String;
    function Stripped_Leading  (The_Character      : in Character; 
                                From_The_String    : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return String;
    function Stripped_Trailing (The_Character      : in Character; 
                                From_The_String    : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return String;
                                                   
    function Number_Of         (The_Character      : in Character; 
                                In_The_String      : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return Natural;
    function Number_Of         (The_String         : in String; 
                                In_The_String      : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return Natural;
    function Location_Of       (The_Character      : in Character; 
                                In_The_String      : in String; 
                                Case_Sensitive     : in Boolean := True; 
                                Forward            : in Boolean := True) 
                                                                return Natural;
    function Is_Equal          (Left               : in String; 
                                Right              : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return Boolean;
    function Is_Less_Than      (Left               : in String; 
                                Right              : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return Boolean;
    function Is_Greater_Than   (Left               : in String; 
                                Right              : in String; 
                                Case_Sensitive     : in Boolean := True) 
                                                                return Boolean;

    Lexical_Error : exception;

end String_Utilities;

--              Original Booch Components (Ada 83 version)
--                               
-- Copyright (C) 2000 Grady Booch
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but 
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU General Public License.  
--
-- The book _SOFTWARE_COMPONENTS_WITH_Ada__Structures,_Tools,_and_Subsystems_
-- ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage 
-- of this software.
--
-- The Ada 83 version of the components is the exact version described in
-- the book mentioned above.  The Ada 95 elaboration version differs only in
-- that each component unit is a child of a root package named "Booch" and
-- each package declaration includes the most appropriate elaboration control
-- pragma.  In addition, the Ada 95 child iteration version eliminates the
-- distinct iterator and noniterator forms for components that had them and
-- instead makes the associated "Iterate" procedures available as children of
-- those components.  More enhanced versions may be produced in the future.
--
-- The Original Booch Components are actively maintained and enhanced
-- by Vincent Marciante and Samuel T. Harris and may be found at the 
-- AdaPower web site (http://www.adapower.com) provided by David Botton.
