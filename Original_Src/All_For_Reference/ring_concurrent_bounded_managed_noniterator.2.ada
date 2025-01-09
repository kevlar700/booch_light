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

package body Ring_Concurrent_Bounded_Managed_Noniterator is

    procedure Seize (The_Ring : in Ring) is
    begin
        Semaphore.Seize (The_Ring.Guard);
    end Seize;

    procedure Release (The_Ring : in Ring) is
    begin
        Semaphore.Release (The_Ring.Guard);
    end Release;

    procedure Copy (From_The_Ring : in Ring; To_The_Ring : in out Ring) is
    begin
        Seize (From_The_Ring);
        Seize (To_The_Ring);
        if From_The_Ring.The_Back > To_The_Ring.The_Size then
            Release (From_The_Ring);
            Release (To_The_Ring);
            raise Overflow;
        elsif From_The_Ring.The_Back = 0 then
            To_The_Ring.The_Top := 0;
            To_The_Ring.The_Back := 0;
            To_The_Ring.The_Mark := 0;
        else
            To_The_Ring.The_Items (1 .. From_The_Ring.The_Back) :=
               From_The_Ring.The_Items (1 .. From_The_Ring.The_Back);
            To_The_Ring.The_Top := From_The_Ring.The_Top;
            To_The_Ring.The_Back := From_The_Ring.The_Back;
            To_The_Ring.The_Mark := From_The_Ring.The_Mark;
        end if;
        Release (From_The_Ring);
        Release (To_The_Ring);
    end Copy;

    procedure Clear (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        The_Ring.The_Top := 0;
        The_Ring.The_Back := 0;
        The_Ring.The_Mark := 0;
        Release (The_Ring);
    end Clear;

    procedure Insert (The_Item : in Item; In_The_Ring : in out Ring) is
    begin
        Seize (In_The_Ring);
        if In_The_Ring.The_Back = In_The_Ring.The_Size then
            Release (In_The_Ring);
            raise Overflow;
        elsif In_The_Ring.The_Back = 0 then
            In_The_Ring.The_Top := 1;
            In_The_Ring.The_Back := 1;
            In_The_Ring.The_Mark := 1;
            In_The_Ring.The_Items (1) := The_Item;
        else
            In_The_Ring.The_Items ((In_The_Ring.The_Top + 1) ..
                                      (In_The_Ring.The_Back + 1)) :=
               In_The_Ring.The_Items (In_The_Ring.The_Top ..
                                         In_The_Ring.The_Back);
            In_The_Ring.The_Items (In_The_Ring.The_Top) := The_Item;
            In_The_Ring.The_Back := In_The_Ring.The_Back + 1;
            if In_The_Ring.The_Mark >= In_The_Ring.The_Top then
                In_The_Ring.The_Mark := In_The_Ring.The_Mark + 1;
            end if;
        end if;
        Release (In_The_Ring);
    end Insert;

    procedure Pop (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        if The_Ring.The_Back = 0 then
            Release (The_Ring);
            raise Underflow;
        elsif The_Ring.The_Back = 1 then
            The_Ring.The_Top := 0;
            The_Ring.The_Back := 0;
            The_Ring.The_Mark := 0;
        else
            The_Ring.The_Items (The_Ring.The_Top .. (The_Ring.The_Back - 1)) :=
               The_Ring.The_Items ((The_Ring.The_Top + 1) .. The_Ring.The_Back);
            The_Ring.The_Back := The_Ring.The_Back - 1;
            if The_Ring.The_Top > The_Ring.The_Back then
                if The_Ring.The_Top = The_Ring.The_Mark then
                    The_Ring.The_Mark := 1;
                end if;
                The_Ring.The_Top := 1;
            else
                if The_Ring.The_Mark > The_Ring.The_Top then
                    The_Ring.The_Mark := The_Ring.The_Mark - 1;
                end if;
            end if;
        end if;
        Release (The_Ring);
    end Pop;

    procedure Rotate (The_Ring : in out Ring;
                      In_The_Direction : in Direction) is
    begin
        Seize (The_Ring);
        if The_Ring.The_Back = 0 then
            Release (The_Ring);
            raise Rotate_Error;
        elsif In_The_Direction = Forward then
            The_Ring.The_Top := The_Ring.The_Top + 1;
            if The_Ring.The_Top > The_Ring.The_Back then
                The_Ring.The_Top := 1;
            end if;
        else
            The_Ring.The_Top := The_Ring.The_Top - 1;
            if The_Ring.The_Top = 0 then
                The_Ring.The_Top := The_Ring.The_Back;
            end if;
        end if;
        Release (The_Ring);
    end Rotate;

    procedure Mark (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        The_Ring.The_Mark := The_Ring.The_Top;
        Release (The_Ring);
    end Mark;

    procedure Rotate_To_Mark (The_Ring : in out Ring) is
    begin
        Seize (The_Ring);
        The_Ring.The_Top := The_Ring.The_Mark;
        Release (The_Ring);
    end Rotate_To_Mark;

    function Is_Equal (Left : in Ring; Right : in Ring) return Boolean is
        Left_Index : Natural;
        Right_Index : Natural;
        Result : Boolean;
    begin
        Seize (Left);
        Seize (Right);
        Left_Index := Left.The_Top;
        Right_Index := Right.The_Top;
        if Left.The_Back /= Right.The_Back then
            Release (Left);
            Release (Right);
            return False;
        elsif Left.The_Items (Left_Index) /= Right.The_Items (Right_Index) then
            Release (Left);
            Release (Right);
            return False;
        elsif (Left.The_Mark = Left_Index) and then
              (Right.The_Mark /= Right_Index) then
            Release (Left);
            Release (Right);
            return False;
        else
            Left_Index := Left_Index + 1;
            if Left_Index > Left.The_Back then
                Left_Index := 1;
            end if;
            Right_Index := Right_Index + 1;
            if Right_Index > Right.The_Back then
                Right_Index := 1;
            end if;
            while Left_Index /= Left.The_Top loop
                if Left.The_Items (Left_Index) /=
                   Right.The_Items (Right_Index) then
                    Release (Left);
                    Release (Right);
                    return False;
                elsif (Left.The_Mark = Left_Index) and then
                      (Right.The_Mark /= Right_Index) then
                    Release (Left);
                    Release (Right);
                    return False;
                else
                    Left_Index := Left_Index + 1;
                    if Left_Index > Left.The_Back then
                        Left_Index := 1;
                    end if;
                    Right_Index := Right_Index + 1;
                    if Right_Index > Right.The_Back then
                        Right_Index := 1;
                    end if;
                end if;
            end loop;
            Result := Right_Index = Right.The_Top;
            Release (Left);
            Release (Right);
            return Result;
        end if;
    exception
        when Constraint_Error =>
            Result := Left.The_Top = Right.The_Top;
            Release (Left);
            Release (Right);
            return Result;
    end Is_Equal;

    function Extent_Of (The_Ring : in Ring) return Natural is
        Count : Natural;
    begin
        Seize (The_Ring);
        Count := The_Ring.The_Back;
        Release (The_Ring);
        return Count;
    end Extent_Of;

    function Is_Empty (The_Ring : in Ring) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Ring);
        Result := The_Ring.The_Back = 0;
        Release (The_Ring);
        return Result;
    end Is_Empty;

    function Top_Of (The_Ring : in Ring) return Item is
        Temporary_Item : Item;
    begin
        Seize (The_Ring);
        Temporary_Item := The_Ring.The_Items (The_Ring.The_Top);
        Release (The_Ring);
        return Temporary_Item;
    exception
        when Constraint_Error =>
            Release (The_Ring);
            raise Underflow;
    end Top_Of;

    function At_Mark (The_Ring : in Ring) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Ring);
        Result := The_Ring.The_Top = The_Ring.The_Mark;
        Release (The_Ring);
        return Result;
    end At_Mark;

end Ring_Concurrent_Bounded_Managed_Noniterator;
