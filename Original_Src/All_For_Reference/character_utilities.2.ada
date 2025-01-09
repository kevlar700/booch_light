--                       Original Booch Components
--
--                            (Ada 83 version)
--                               
--                     Copyright (C) 2000 Grady Booch
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

package body Character_Utilities is

    procedure Make_Uppercase (The_Character : in out Character) is
    begin
        if The_Character in Lowercase_Character then
            The_Character := Character'Val 
                                (Character'Pos (The_Character) - 
                                 (Character'Pos ('a') - Character'Pos ('A')));
        end if;
    end Make_Uppercase;

    procedure Make_Lowercase (The_Character : in out Character) is
    begin
        if The_Character in Uppercase_Character then
            The_Character := Character'Val 
                                (Character'Pos (The_Character) + 
                                 (Character'Pos ('a') - Character'Pos ('A')));
        end if;
    end Make_Lowercase;

    function Uppercase (The_Character : in Character) return Character is
    begin
        if The_Character in Lowercase_Character then
            return Character'Val (Character'Pos (The_Character) - 
                                  (Character'Pos ('a') - Character'Pos ('A')));
        else
            return The_Character;
        end if;
    end Uppercase;

    function Lowercase (The_Character : in Character) return Character is
    begin
        if The_Character in Uppercase_Character then
            return Character'Val (Character'Pos (The_Character) + 
                                  (Character'Pos ('a') - Character'Pos ('A')));
        else
            return The_Character;
        end if;
    end Lowercase;

    function Is_Control (The_Character : in Character) return Boolean is
    begin
        return (The_Character in Control_Character);
    end Is_Control;

    function Is_Graphic (The_Character : in Character) return Boolean is
    begin
        return (The_Character in Graphic_Character);
    end Is_Graphic;

    function Is_Uppercase (The_Character : in Character) return Boolean is
    begin
        return (The_Character in Uppercase_Character);
    end Is_Uppercase;

    function Is_Lowercase (The_Character : in Character) return Boolean is
    begin
        return (The_Character in Lowercase_Character);
    end Is_Lowercase;

    function Is_Digit (The_Character : in Character) return Boolean is
    begin
        return (The_Character in Digit_Character);
    end Is_Digit;

    function Is_Alphabetic (The_Character : in Character) return Boolean is
    begin
        return ((The_Character in Uppercase_Character) or else 
                (The_Character in Lowercase_Character));
    end Is_Alphabetic;

    function Is_Alphanumeric (The_Character : in Character) return Boolean is
    begin
        return ((The_Character in Uppercase_Character) or else 
                (The_Character in Lowercase_Character) or else 
                (The_Character in Digit_Character));
    end Is_Alphanumeric;

    function Is_Special (The_Character : in Character) return Boolean is
    begin
        return ((The_Character in Graphic_Character) and then 
                not Is_Alphanumeric (The_Character));
    end Is_Special;

    function Value_Of (The_Character : in Character) return Digit is
    begin
        if The_Character in Digit_Character then
            return (Character'Pos (The_Character) - Character'Pos ('0'));
        elsif The_Character in 'A' .. 'F' then
            return (Character'Pos (The_Character) - Character'Pos ('A') + 10);
        else
            raise Lexical_Error;
        end if;
    end Value_Of;

    function Image_Of (The_Digit : in Digit) return Character is
    begin
        if The_Digit < 10 then
            return Character'Val (The_Digit + Character'Pos ('0'));
        else
            return Character'Val (The_Digit + Character'Pos ('A') - 10);
        end if;
    end Image_Of;

    function Index_Of (The_Character : in Character) return Letter is
    begin
        if The_Character in Uppercase_Character then
            return (Character'Pos (The_Character) - Character'Pos ('A') + 1);
        elsif The_Character in Lowercase_Character then
            return (Character'Pos (The_Character) - Character'Pos ('a') + 1);
        else
            raise Lexical_Error;
        end if;
    end Index_Of;

    function Uppercase_Of (The_Letter : in Letter) return Character is
    begin
        return Character'Val (The_Letter + Character'Pos ('A') - 1);
    end Uppercase_Of;

    function Lowercase_Of (The_Letter : in Letter) return Character is
    begin
        return Character'Val (The_Letter + Character'Pos ('a') - 1);
    end Lowercase_Of;

    function Is_Equal (Left : in Character; 
                       Right : in Character; 
                       Case_Sensitive : in Boolean := True) return Boolean is
    begin
        if Case_Sensitive then
            return (Left = Right);
        else
            return (Uppercase (Left) = Uppercase (Right));
        end if;
    end Is_Equal;

    function Is_Less_Than 
                (Left : in Character; 
                 Right : in Character; 
                 Case_Sensitive : in Boolean := True) return Boolean is
    begin
        if Case_Sensitive then
            return (Left < Right);
        else
            return (Uppercase (Left) < Uppercase (Right));
        end if;
    end Is_Less_Than;

    function Is_Greater_Than 
                (Left : in Character; 
                 Right : in Character; 
                 Case_Sensitive : in Boolean := True) return Boolean is
    begin
        if Case_Sensitive then
            return (Left > Right);
        else
            return (Uppercase (Left) > Uppercase (Right));
        end if;
    end Is_Greater_Than;

end Character_Utilities;
