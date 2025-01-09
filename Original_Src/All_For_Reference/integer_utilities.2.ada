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
package body Integer_Utilities is

    Blank : constant Character := ' ';
    Plus : constant Character := '+';
    Minus : constant Character := '-';

    function Min (Left : in Number; Right : in Number) return Number is
    begin
        if Left > Right then
            return Right;
        else
            return Left;
        end if;
    end Min;

    function Min (The_Numbers : in Numbers) return Number is
        Smallest_Number : Number := The_Numbers (The_Numbers'First);
    begin
        for Index in (The_Numbers'First + 1) .. The_Numbers'Last loop
            if The_Numbers (Index) < Smallest_Number then
                Smallest_Number := The_Numbers (Index);
            end if;
        end loop;
        return Smallest_Number;
    end Min;

    function Max (Left : in Number; Right : in Number) return Number is
    begin
        if Left > Right then
            return Left;
        else
            return Right;
        end if;
    end Max;

    function Max (The_Numbers : in Numbers) return Number is
        Largest_Number : Number := The_Numbers (The_Numbers'First);
    begin
        for Index in (The_Numbers'First + 1) .. The_Numbers'Last loop
            if The_Numbers (Index) > Largest_Number then
                Largest_Number := The_Numbers (Index);
            end if;
        end loop;
        return Largest_Number;
    end Max;

    function Is_Positive (The_Number : in Number) return Boolean is
    begin
        return (The_Number > 0);
    end Is_Positive;

    function Is_Natural (The_Number : in Number) return Boolean is
    begin
        return (The_Number >= 0);
    end Is_Natural;

    function Is_Negative (The_Number : in Number) return Boolean is
    begin
        return (The_Number < 0);
    end Is_Negative;

    function Is_Zero (The_Number : in Number) return Boolean is
    begin
        return (The_Number = 0);
    end Is_Zero;

    function Is_Odd (The_Number : in Number) return Boolean is
    begin
        return (The_Number mod 2 /= 0);
    end Is_Odd;

    function Is_Even (The_Number : in Number) return Boolean is
    begin
        return (The_Number mod 2 = 0);
    end Is_Even;

    function Based_Image (The_Number : in Number; With_The_Base : in Base) 
                         return String is
        Result : Number;
        The_Remainder : Natural;
    begin
        Result := The_Number / Number (With_The_Base);
        The_Remainder := Natural (The_Number mod Number (With_The_Base));
        if Result = 0 then
            return "" & Character_Utilities.Image_Of (The_Remainder);
        else
            return Based_Image (Result, With_The_Base) & 
                      Character_Utilities.Image_Of (The_Remainder);
        end if;
    end Based_Image;

    function Image_Of (The_Number : in Number; With_The_Base : in Base := 10) 
                      return String is
    begin
        if The_Number < 0 then
            return '-' & Based_Image (abs (The_Number), With_The_Base);
        else
            return ' ' & Based_Image (The_Number, With_The_Base);
        end if;
    end Image_Of;

    function Value_Of (The_Character : in Character; With_The_Base : in Base) 
                      return Number is
        Result : Number;
    begin
        Result := Number (Character_Utilities.Value_Of (The_Character));
        if Result > Number (With_The_Base) then
            raise Lexical_Error;
        else
            return Result;
        end if;
    exception
        when Character_Utilities.Lexical_Error =>
            raise Lexical_Error;
    end Value_Of;

    function Value_Of (The_Image : in String; With_The_Base : in Base := 10) 
                      return Number is
        Result : Number := 0;
        The_Power : Natural := 0;
    begin
        for Index in reverse (The_Image'First + 1) .. The_Image'Last loop
            Result := Result + (Value_Of (The_Image (Index), With_The_Base) * 
                                (Number (With_The_Base) ** The_Power));
            The_Power := The_Power + 1;
        end loop;
        if (The_Image (The_Image'First) = Plus) or 
           (The_Image (The_Image'First) = Blank) then
            return Result;
        elsif The_Image (The_Image'First) = Minus then
            return -Result;
        else
            Result := Result + (Value_Of (The_Image (The_Image'First), 
                                          With_The_Base) * 
                                (Number (With_The_Base) ** The_Power));
            return Result;
        end if;
    end Value_Of;

end Integer_Utilities;
