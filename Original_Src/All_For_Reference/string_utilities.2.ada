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

with Character_Utilities;
package body String_Utilities is

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

    procedure Replace (The_Character : in Character; 
                       With_The_Character : in Character; 
                       In_The_String : in out String; 
                       Case_Sensitive : in Boolean := True) is
    begin
        for Index in In_The_String'Range loop
            if Character_Utilities.Is_Equal 
                  (The_Character, In_The_String (Index), Case_Sensitive) then
                In_The_String (Index) := With_The_Character;
            end if;
        end loop;
    end Replace;

    function Uppercase (The_String : in String) return String is
        Temporary_String : String (The_String'Range) := The_String;
    begin
        Make_Uppercase (Temporary_String);
        return Temporary_String;
    end Uppercase;

    function Lowercase (The_String : in String) return String is
        Temporary_String : String (The_String'Range) := The_String;
    begin
        Make_Lowercase (Temporary_String);
        return Temporary_String;
    end Lowercase;

    function Capitalized (The_String : in String) return String is
    begin
        return (Character_Utilities.Uppercase (The_String (The_String'First)) & 
                The_String ((The_String'First + 1) .. The_String'Last));
    end Capitalized;

    function Uncapitalized (The_String : in String) return String is
    begin
        return (Character_Utilities.Lowercase (The_String (The_String'First)) & 
                The_String ((The_String'First + 1) .. The_String'Last));
    end Uncapitalized;

    function Replaced (The_Character : in Character; 
                       With_The_Character : in Character; 
                       In_The_String : in String; 
                       Case_Sensitive : in Boolean := True) return String is
        Temporary_String : String (In_The_String'Range) := In_The_String;
    begin
        Replace (The_Character, With_The_Character, Temporary_String,  
                 Case_Sensitive);
        return Temporary_String;
    end Replaced;

    function Is_Null (The_String : in String) return Boolean is
    begin
        return (The_String = "");
    end Is_Null;

    function Is_Control (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Control (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Control;

    function Is_Graphic (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Graphic (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Graphic;

    function Is_Uppercase (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Uppercase (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Uppercase;

    function Is_Lowercase (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Lowercase (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Lowercase;

    function Is_Digit (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Digit (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Digit;

    function Is_Alphabetic (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Alphabetic (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Alphabetic;

    function Is_Alphanumeric (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Alphanumeric (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Alphanumeric;

    function Is_Special (The_String : in String) return Boolean is
    begin
        for Index in The_String'Range loop
            if not Character_Utilities.Is_Special (The_String (Index)) then
                return False;
            end if;
        end loop;
        return (The_String /= "");
    end Is_Special;

    function Centered (The_String : in String; 
                       In_The_Width : in Positive; 
                       With_The_Filler : in Character) return String is
        Left_Margin : Natural;
        Right_Margin : Natural;
    begin
        Left_Margin := (In_The_Width - The_String'Length) / 2;
        Right_Margin := In_The_Width - The_String'Length - Left_Margin;
        return (String'(1 .. Left_Margin => With_The_Filler) & The_String & 
                String'(1 .. Right_Margin => With_The_Filler));
    exception
        when Constraint_Error =>
            raise Lexical_Error;
    end Centered;

    function Left_Justified (The_String : in String; 
                             In_The_Width : in Positive; 
                             With_The_Filler : in Character) return String is
        Right_Margin : Natural;
    begin
        Right_Margin := In_The_Width - The_String'Length;
        return (The_String & String'(1 .. Right_Margin => With_The_Filler));
    exception
        when Constraint_Error =>
            raise Lexical_Error;
    end Left_Justified;

    function Right_Justified (The_String : in String; 
                              In_The_Width : in Positive; 
                              With_The_Filler : in Character) return String is
        Left_Margin : Natural;
    begin
        Left_Margin := In_The_Width - The_String'Length;
        return (String'(1 .. Left_Margin => With_The_Filler) & The_String);
    exception
        when Constraint_Error =>
            raise Lexical_Error;
    end Right_Justified;

    function Stripped (The_Character : in Character; 
                       From_The_String : in String; 
                       Case_Sensitive : in Boolean := True) return String is
        Temporary_String : String (From_The_String'Range);
        The_Back : Natural := Temporary_String'First;
    begin
        for Index in From_The_String'Range loop
            if not Character_Utilities.Is_Equal 
                      (The_Character, From_The_String (Index), 
                       Case_Sensitive) then
                Temporary_String (The_Back) := From_The_String (Index);
                The_Back := The_Back + 1;
            end if;
        end loop;
        return Temporary_String (Temporary_String'First .. (The_Back - 1));
    end Stripped;

    function Stripped_Leading 
                (The_Character : in Character; 
                 From_The_String : in String; 
                 Case_Sensitive : in Boolean := True) return String is
        The_Front : Natural := From_The_String'First;
    begin
        for Index in From_The_String'Range loop
            if Character_Utilities.Is_Equal 
                  (The_Character, From_The_String (Index), Case_Sensitive) then
                The_Front := The_Front + 1;
            else
                return From_The_String (The_Front .. From_The_String'Last);
            end if;
        end loop;
        return "";
    end Stripped_Leading;

    function Stripped_Trailing (The_Character : in Character; 
                                From_The_String : in String; 
                                Case_Sensitive : in Boolean := True)  
                               return String is
        The_Back : Natural := From_The_String'Last;
    begin
        for Index in reverse From_The_String'Range loop
            if Character_Utilities.Is_Equal 
                  (The_Character, From_The_String (Index), Case_Sensitive) then
                The_Back := The_Back - 1;
            else
                return From_The_String (From_The_String'First .. The_Back);
            end if;
        end loop;
        return "";
    end Stripped_Trailing;

    function Number_Of (The_Character : in Character; 
                        In_The_String : in String; 
                        Case_Sensitive : in Boolean := True) return Natural is
        Count : Natural := 0;
    begin
        for Index in In_The_String'Range loop
            if Character_Utilities.Is_Equal 
                  (The_Character, In_The_String (Index), Case_Sensitive) then
                Count := Count + 1;
            end if;
        end loop;
        return Count;
    end Number_Of;

    function Number_Of (The_String : in String; 
                        In_The_String : in String; 
                        Case_Sensitive : in Boolean := True) return Natural is
        Count : Natural := 0;
    begin
        for Index in In_The_String'First .. 
                        (In_The_String'Last - The_String'Length + 1) loop
            if Is_Equal (The_String, 
                         In_The_String (Index .. 
                                           (Index + The_String'Length - 1)), 
                         Case_Sensitive) then
                Count := Count + 1;
            end if;
        end loop;
        return Count;
    end Number_Of;

    function Location_Of (The_Character : in Character; 
                          In_The_String : in String; 
                          Case_Sensitive : in Boolean := True; 
                          Forward : in Boolean := True) return Natural is
    begin
        if Forward then
            for Index in In_The_String'Range loop
                if Character_Utilities.Is_Equal 
                      (The_Character, In_The_String (Index), 
                       Case_Sensitive) then
                    return Index;
                end if;
            end loop;
            return 0;
        else
            for Index in reverse In_The_String'Range loop
                if Character_Utilities.Is_Equal 
                      (The_Character, In_The_String (Index), 
                       Case_Sensitive) then
                    return Index;
                end if;
            end loop;
            return 0;
        end if;
    end Location_Of;

    function Is_Equal (Left : in String; 
                       Right : in String; 
                       Case_Sensitive : in Boolean := True) return Boolean is
    begin
        if Left'Length /= Right'Length then
            return False;
        else
            for Index in Left'Range loop
                if not Character_Utilities.Is_Equal 
                          (Left (Index), Right 
                                            (Right'First + Index - Left'First), 
                           Case_Sensitive) then
                    return False;
                end if;
            end loop;
            return True;
        end if;
    end Is_Equal;

    function Is_Less_Than 
                (Left : in String; 
                 Right : in String; 
                 Case_Sensitive : in Boolean := True) return Boolean is
    begin
        for Index in Left'Range loop
            if Character_Utilities.Is_Less_Than 
                  (Left (Index), Right (Right'First + Index - Left'First), 
                   Case_Sensitive) then
                return True;
            end if;
        end loop;
        return (Left'Length < Right'Length);
    exception
        when Constraint_Error =>
            return False;
    end Is_Less_Than;

    function Is_Greater_Than 
                (Left : in String; 
                 Right : in String; 
                 Case_Sensitive : in Boolean := True) return Boolean is
    begin
        for Index in Left'Range loop
            if Character_Utilities.Is_Greater_Than 
                  (Left (Index), Right (Right'First + Index - Left'First), 
                   Case_Sensitive) then
                return True;
            end if;
        end loop;
        return False;
    exception
        when Constraint_Error =>
            return (Left'Length > Right'Length);
    end Is_Greater_Than;

end String_Utilities;
