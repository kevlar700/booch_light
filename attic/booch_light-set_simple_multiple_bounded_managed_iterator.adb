--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Set_Simple_Multiple_Bounded_Managed_Iterator is

    procedure Seize_For_Reading (The_Set : in Set) is
    begin
        Monitor.Start_Reading (The_Set.Guard);
    end Seize_For_Reading;

    procedure Release_For_Reading (The_Set : in Set) is
    begin
        Monitor.Stop_Reading (The_Set.Guard);
    end Release_For_Reading;

    procedure Seize_For_Writing (The_Set : in Set) is
    begin
        Monitor.Start_Writing (The_Set.Guard);
    end Seize_For_Writing;

    procedure Release_For_Writing (The_Set : in Set) is
    begin
        Monitor.Stop_Writing (The_Set.Guard);
    end Release_For_Writing;

    procedure Copy (From_The_Set : in Set; To_The_Set : in out Set) is
    begin
        Seize_For_Reading (From_The_Set);
        Seize_For_Writing (To_The_Set);
        if From_The_Set.The_Back > To_The_Set.The_Size then
            Release_For_Reading (From_The_Set);
            Release_For_Writing (To_The_Set);
            raise Overflow;
        else
            To_The_Set.The_Items (1 .. From_The_Set.The_Back) :=
               From_The_Set.The_Items (1 .. From_The_Set.The_Back);
            To_The_Set.The_Back := From_The_Set.The_Back;
            Release_For_Reading (From_The_Set);
            Release_For_Writing (To_The_Set);
        end if;
    end Copy;

    procedure Clear (The_Set : in out Set) is
    begin
        Seize_For_Writing (The_Set);
        The_Set.The_Back := 0;
        Release_For_Writing (The_Set);
    end Clear;

    procedure Add (The_Item : in Item; To_The_Set : in out Set) is
    begin
        Seize_For_Writing (To_The_Set);
        for Index in 1 .. To_The_Set.The_Back loop
            if The_Item = To_The_Set.The_Items (Index) then
                Release_For_Writing (To_The_Set);
                raise Item_Is_In_Set;
            end if;
        end loop;
        To_The_Set.The_Items (To_The_Set.The_Back + 1) := The_Item;
        To_The_Set.The_Back := To_The_Set.The_Back + 1;
        Release_For_Writing (To_The_Set);
    exception
        when Constraint_Error =>
            Release_For_Writing (To_The_Set);
            raise Overflow;
    end Add;

    procedure Remove (The_Item : in Item; From_The_Set : in out Set) is
    begin
        Seize_For_Writing (From_The_Set);
        for Index in 1 .. From_The_Set.The_Back loop
            if The_Item = From_The_Set.The_Items (Index) then
                From_The_Set.The_Items (Index .. (From_The_Set.The_Back - 1)) :=
                   From_The_Set.The_Items ((Index + 1) ..
                                              From_The_Set.The_Back);
                From_The_Set.The_Back := From_The_Set.The_Back - 1;
                Release_For_Writing (From_The_Set);
                return;
            end if;
        end loop;
        Release_For_Writing (From_The_Set);
        raise Item_Is_Not_In_Set;
    end Remove;

    procedure Union (Of_The_Set : in Set;
                     And_The_Set : in Set;
                     To_The_Set : in out Set) is
        To_Index : Natural;
        To_Back : Natural;
    begin
        Seize_For_Reading (Of_The_Set);
        Seize_For_Reading (And_The_Set);
        Seize_For_Writing (To_The_Set);
        To_The_Set.The_Items (1 .. Of_The_Set.The_Back) :=
           Of_The_Set.The_Items (1 .. Of_The_Set.The_Back);
        To_The_Set.The_Back := Of_The_Set.The_Back;
        To_Back := To_The_Set.The_Back;
        for And_Index in 1 .. And_The_Set.The_Back loop
            To_Index := To_Back;
            while To_Index > 0 loop
                if To_The_Set.The_Items (To_Index) =
                   And_The_Set.The_Items (And_Index) then
                    exit;
                else
                    To_Index := To_Index - 1;
                end if;
            end loop;
            if To_Index = 0 then
                To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                   And_The_Set.The_Items (And_Index);
                To_The_Set.The_Back := To_The_Set.The_Back + 1;
            end if;
        end loop;
        Release_For_Reading (Of_The_Set);
        Release_For_Reading (And_The_Set);
        Release_For_Writing (To_The_Set);
    exception
        when Constraint_Error =>
            Release_For_Reading (Of_The_Set);
            Release_For_Reading (And_The_Set);
            Release_For_Writing (To_The_Set);
            raise Overflow;
    end Union;

    procedure Intersection (Of_The_Set : in Set;
                            And_The_Set : in Set;
                            To_The_Set : in out Set) is
        And_Index : Natural;
    begin
        Seize_For_Reading (Of_The_Set);
        Seize_For_Reading (And_The_Set);
        Seize_For_Writing (To_The_Set);
        To_The_Set.The_Back := 0;
        for Of_Index in 1 .. Of_The_Set.The_Back loop
            And_Index := And_The_Set.The_Back;
            while And_Index > 0 loop
                if Of_The_Set.The_Items (Of_Index) =
                   And_The_Set.The_Items (And_Index) then
                    To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                       Of_The_Set.The_Items (Of_Index);
                    To_The_Set.The_Back := To_The_Set.The_Back + 1;
                    exit;
                else
                    And_Index := And_Index - 1;
                end if;
            end loop;
        end loop;
        Release_For_Reading (Of_The_Set);
        Release_For_Reading (And_The_Set);
        Release_For_Writing (To_The_Set);
    exception
        when Constraint_Error =>
            Release_For_Reading (Of_The_Set);
            Release_For_Reading (And_The_Set);
            Release_For_Writing (To_The_Set);
            raise Overflow;
    end Intersection;

    procedure Difference (Of_The_Set : in Set;
                          And_The_Set : in Set;
                          To_The_Set : in out Set) is
        And_Index : Natural;
    begin
        Seize_For_Reading (Of_The_Set);
        Seize_For_Reading (And_The_Set);
        Seize_For_Writing (To_The_Set);
        To_The_Set.The_Back := 0;
        for Of_Index in 1 .. Of_The_Set.The_Back loop
            And_Index := And_The_Set.The_Back;
            while And_Index > 0 loop
                if Of_The_Set.The_Items (Of_Index) =
                   And_The_Set.The_Items (And_Index) then
                    exit;
                else
                    And_Index := And_Index - 1;
                end if;
            end loop;
            if And_Index = 0 then
                To_The_Set.The_Items (To_The_Set.The_Back + 1) :=
                   Of_The_Set.The_Items (Of_Index);
                To_The_Set.The_Back := To_The_Set.The_Back + 1;
            end if;
        end loop;
        Release_For_Reading (Of_The_Set);
        Release_For_Reading (And_The_Set);
        Release_For_Writing (To_The_Set);
    exception
        when Constraint_Error =>
            Release_For_Reading (Of_The_Set);
            Release_For_Reading (And_The_Set);
            Release_For_Writing (To_The_Set);
            raise Overflow;
    end Difference;

    function Is_Equal (Left : in Set; Right : in Set) return Boolean is
        Right_Index : Natural;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        if Left.The_Back /= Right.The_Back then
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return False;
        else
            for Left_Index in 1 .. Left.The_Back loop
                Right_Index := Right.The_Back;
                while Right_Index > 0 loop
                    if Left.The_Items (Left_Index) =
                       Right.The_Items (Right_Index) then
                        exit;
                    else
                        Right_Index := Right_Index - 1;
                    end if;
                end loop;
                if Right_Index = 0 then
                    Release_For_Reading (Left);
                    Release_For_Reading (Right);
                    return False;
                end if;
            end loop;
            Release_For_Reading (Left);
            Release_For_Reading (Right);
            return True;
        end if;
    end Is_Equal;

    function Extent_Of (The_Set : in Set) return Natural is
        Count : Natural;
    begin
        Seize_For_Reading (The_Set);
        Count := The_Set.The_Back;
        Release_For_Reading (The_Set);
        return Count;
    end Extent_Of;

    function Is_Empty (The_Set : in Set) return Boolean is
        Result : Boolean;
    begin
        Seize_For_Reading (The_Set);
        Result := The_Set.The_Back = 0;
        Release_For_Reading (The_Set);
        return Result;
    end Is_Empty;

    function Is_A_Member
                (The_Item : in Item; Of_The_Set : in Set) return Boolean is
    begin
        Seize_For_Reading (Of_The_Set);
        for Index in 1 .. Of_The_Set.The_Back loop
            if Of_The_Set.The_Items (Index) = The_Item then
                Release_For_Reading (Of_The_Set);
                return True;
            end if;
        end loop;
        Release_For_Reading (Of_The_Set);
        return False;
    end Is_A_Member;

    function Is_A_Subset (Left : in Set; Right : in Set) return Boolean is
        Right_Index : Natural;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
                if Left.The_Items (Left_Index) =
                   Right.The_Items (Right_Index) then
                    exit;
                else
                    Right_Index := Right_Index - 1;
                end if;
            end loop;
            if Right_Index = 0 then
                Release_For_Reading (Left);
                Release_For_Reading (Right);
                return False;
            end if;
        end loop;
        Release_For_Reading (Left);
        Release_For_Reading (Right);
        return True;
    end Is_A_Subset;

    function Is_A_Proper_Subset
                (Left : in Set; Right : in Set) return Boolean is
        Right_Index : Natural;
        Result : Boolean;
    begin
        Seize_For_Reading (Left);
        Seize_For_Reading (Right);
        for Left_Index in 1 .. Left.The_Back loop
            Right_Index := Right.The_Back;
            while Right_Index > 0 loop
                if Left.The_Items (Left_Index) =
                   Right.The_Items (Right_Index) then
                    exit;
                else
                    Right_Index := Right_Index - 1;
                end if;
            end loop;
            if Right_Index = 0 then
                Release_For_Reading (Left);
                Release_For_Reading (Right);
                return False;
            end if;
        end loop;
        Result := Left.The_Back < Right.The_Back;
        Release_For_Reading (Left);
        Release_For_Reading (Right);
        return Result;
    end Is_A_Proper_Subset;

    procedure Iterate (Over_The_Set : in Set) is
        Continue : Boolean;
    begin
        Seize_For_Reading (Over_The_Set);
        for The_Iterator in 1 .. Over_The_Set.The_Back loop
            Process (Over_The_Set.The_Items (The_Iterator), Continue);
            exit when not Continue;
        end loop;
        Release_For_Reading (Over_The_Set);
    end Iterate;

end Booch_Light.Set_Simple_Multiple_Bounded_Managed_Iterator;

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
