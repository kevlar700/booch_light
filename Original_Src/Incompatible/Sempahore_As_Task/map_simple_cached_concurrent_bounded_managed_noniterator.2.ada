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

package body Map_Simple_Cached_Concurrent_Bounded_Managed_Noniterator is

    procedure Find (The_Domain : in Domain;
                    In_The_Map : in Map;
                    The_Bucket : out Natural) is
        Initial_Probe : Natural := Hash_Of (The_Domain) mod In_The_Map.The_Size;
        Temporary_Index : Positive;
        Temporary_Bucket : Natural;
    begin
        Temporary_Bucket := 0;
        for Index in In_The_Map.The_Items'Range loop
            Temporary_Index := ((Index + Initial_Probe - 2) mod
                                In_The_Map.The_Size) + 1;
            case In_The_Map.The_Items (Temporary_Index).The_State is
                when Empty =>
                    if Temporary_Bucket = 0 then
                        Temporary_Bucket := Temporary_Index;
                    end if;
                    The_Bucket := Temporary_Bucket;
                    return;
                when Deleted =>
                    if Temporary_Bucket = 0 then
                        Temporary_Bucket := Temporary_Index;
                    end if;
                when Bound =>
                    if In_The_Map.The_Items (Temporary_Index).The_Domain =
                       The_Domain then
                        The_Bucket := Temporary_Index;
                        return;
                    end if;
            end case;
        end loop;
        The_Bucket := Temporary_Bucket;
    end Find;

    procedure Seize (The_Map : in Map) is
    begin
        Semaphore.Seize (The_Map.Guard);
    end Seize;

    procedure Release (The_Map : in Map) is
    begin
        Semaphore.Release (The_Map.Guard);
    end Release;

    procedure Copy (From_The_Map : in Map; To_The_Map : in out Map) is
        The_Bucket : Natural;
    begin
        Seize (From_The_Map);
        Seize (To_The_Map);
        if From_The_Map.The_Count > To_The_Map.The_Size then
            Release (From_The_Map);
            Release (To_The_Map);
            raise Overflow;
        else
            for Index in To_The_Map.The_Items'Range loop
                To_The_Map.The_Items (Index).The_State := Empty;
            end loop;
            To_The_Map.The_Count := 0;
            for Index in From_The_Map.The_Items'Range loop
                if From_The_Map.The_Items (Index).The_State = Bound then
                    Find (From_The_Map.The_Items (Index).The_Domain,
                          To_The_Map, The_Bucket);
                    To_The_Map.The_Items (The_Bucket) :=
                       From_The_Map.The_Items (Index);
                end if;
            end loop;
            To_The_Map.The_Count := From_The_Map.The_Count;
            To_The_Map.The_Cache.Is_Current := False;
        end if;
        Release (From_The_Map);
        Release (To_The_Map);
    end Copy;

    procedure Clear (The_Map : in out Map) is
    begin
        Seize (The_Map);
        for Index in The_Map.The_Items'Range loop
            The_Map.The_Items (Index).The_State := Empty;
        end loop;
        The_Map.The_Count := 0;
        The_Map.The_Cache.Is_Current := False;
        Release (The_Map);
    end Clear;

    procedure Bind (The_Domain : in Domain;
                    And_The_Range : in Ranges;
                    In_The_Map : in out Map) is
        The_Bucket : Natural;
    begin
        Seize (In_The_Map);
        Find (The_Domain, In_The_Map, The_Bucket);
        if In_The_Map.The_Items (The_Bucket).The_State = Bound then
            Release (In_The_Map);
            raise Multiple_Binding;
        else
            In_The_Map.The_Items (The_Bucket) :=
               Node'(Bound, The_Domain, And_The_Range);
            In_The_Map.The_Count := In_The_Map.The_Count + 1;
            In_The_Map.The_Cache := Cache'(The_Domain => The_Domain,
                                           The_Range => And_The_Range,
                                           Is_Bound => True,
                                           Is_Current => True);
            Release (In_The_Map);
        end if;
    exception
        when Constraint_Error =>
            Release (In_The_Map);
            raise Overflow;
    end Bind;

    procedure Unbind (The_Domain : in Domain; In_The_Map : in out Map) is
        The_Bucket : Natural;
    begin
        Seize (In_The_Map);
        Find (The_Domain, In_The_Map, The_Bucket);
        if In_The_Map.The_Items (The_Bucket).The_State = Bound then
            In_The_Map.The_Items (The_Bucket).The_State := Deleted;
            In_The_Map.The_Count := In_The_Map.The_Count - 1;
            In_The_Map.The_Cache.The_Domain := The_Domain;
            In_The_Map.The_Cache.Is_Bound := False;
            In_The_Map.The_Cache.Is_Current := True;
            Release (In_The_Map);
        else
            Release (In_The_Map);
            raise Domain_Is_Not_Bound;
        end if;
    exception
        when Constraint_Error =>
            Release (In_The_Map);
            raise Domain_Is_Not_Bound;
    end Unbind;

    function Is_Equal (Left : in Map; Right : in Map) return Boolean is
        Temporary_Index : Natural;
    begin
        Seize (Left);
        Seize (Right);
        if Left.The_Count /= Right.The_Count then
            Release (Left);
            Release (Right);
            return False;
        else
            for Index in 1 .. Left.The_Size loop
                if Left.The_Items (Index).The_State = Bound then
                    Temporary_Index := 0;
                    for Inner_Index in 1 .. Right.The_Size loop
                        if (Right.The_Items (Index).The_State = Bound) and then
                           (Left.The_Items (Index).The_Domain =
                            Right.The_Items (Inner_Index).The_Domain) then
                            Temporary_Index := Inner_Index;
                            exit;
                        end if;
                    end loop;
                    if Left.The_Items (Index).The_Range /=
                       Right.The_Items (Temporary_Index).The_Range then
                        Release (Left);
                        Release (Right);
                        return False;
                    end if;
                end if;
            end loop;
            Release (Left);
            Release (Right);
            return True;
        end if;
    exception
        when Constraint_Error =>
            Release (Left);
            Release (Right);
            return False;
    end Is_Equal;

    function Extent_Of (The_Map : in Map) return Natural is
        The_Result : Natural;
    begin
        Seize (The_Map);  
        The_Result := The_Map.The_Count;
        Release (The_Map);
        return The_Result;
    end Extent_Of;

    function Is_Empty (The_Map : in Map) return Boolean is
        The_Result : Boolean;
    begin
        Seize (The_Map);
        The_Result := The_Map.The_Count = 0;
        Release (The_Map);
        return The_Result;
    end Is_Empty;

    function Is_Bound
                (The_Domain : in Domain; In_The_Map : in Map) return Boolean is
        The_Bucket : Natural;
        The_Result : Boolean;
    begin
        Seize (In_The_Map);
        if In_The_Map.The_Cache.Is_Current and then
           In_The_Map.The_Cache.The_Domain = The_Domain then
            The_Result := In_The_Map.The_Cache.Is_Bound;
            Release (In_The_Map);
            return The_Result;
        else
            Find (The_Domain, In_The_Map, The_Bucket);
            The_Result := (In_The_Map.The_Items (The_Bucket).The_State = Bound);
            Release (In_The_Map);
            return The_Result;
        end if;
    exception
        when Constraint_Error =>
            Release (In_The_Map);
            return False;
    end Is_Bound;

    function Range_Of
                (The_Domain : in Domain; In_The_Map : in Map) return Ranges is
        The_Bucket : Natural;
        The_Result : Ranges;
    begin
        Seize (In_The_Map);
        if In_The_Map.The_Cache.Is_Current and then
           In_The_Map.The_Cache.The_Domain = The_Domain then
            if In_The_Map.The_Cache.Is_Bound then
                The_Result := In_The_Map.The_Cache.The_Range;
                Release (In_The_Map);
                return The_Result;
            else
                Release (In_The_Map);
                raise Domain_Is_Not_Bound;
            end if;
        else
            Find (The_Domain, In_The_Map, The_Bucket);
            if In_The_Map.The_Items (The_Bucket).The_State = Bound then
                The_Result := In_The_Map.The_Items (The_Bucket).The_Range;
                Release (In_The_Map);
                return The_Result;
            else
                Release (In_The_Map);
                raise Domain_Is_Not_Bound;
            end if;  
        end if;
    exception
        when Constraint_Error =>
            Release (In_The_Map);
            raise Domain_Is_Not_Bound;
    end Range_Of;

end Map_Simple_Cached_Concurrent_Bounded_Managed_Noniterator;
