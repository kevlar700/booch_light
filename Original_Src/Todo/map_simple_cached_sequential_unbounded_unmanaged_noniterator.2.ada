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

package body Map_Simple_Cached_Sequential_Unbounded_Unmanaged_Noniterator is

    type Node is
        record
            The_Domain : Domain;
            The_Range : Ranges;
            Next : Structure;
        end record;

    procedure Find (The_Domain : in Domain;
                    In_The_Map : in Map;
                    The_Bucket : out Positive;
                    Previous_Node : in out Structure;
                    Current_Node : in out Structure) is
        Temporary_Bucket : Positive :=
           (Hash_Of (The_Domain) mod Number_Of_Buckets) + 1;
    begin
        The_Bucket := Temporary_Bucket;
        Current_Node := In_The_Map.The_Items (Temporary_Bucket);
        while Current_Node /= null loop
            if Current_Node.The_Domain = The_Domain then
                return;
            else
                Previous_Node := Current_Node;
                Current_Node := Current_Node.Next;
            end if;
        end loop;
    end Find;

    procedure Copy (From_The_Map : in Map; To_The_Map : in out Map) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        for Index in From_The_Map.The_Items'Range loop
            From_Index := From_The_Map.The_Items (Index);
            if From_The_Map.The_Items (Index) = null then
                To_The_Map.The_Items (Index) := null;
            else
                To_The_Map.The_Items (Index) :=
                   new Node'(The_Domain => From_Index.The_Domain,
                             The_Range => From_Index.The_Range,
                             Next => null);
                To_Index := To_The_Map.The_Items (Index);
                From_Index := From_Index.Next;
                while From_Index /= null loop
                    To_Index.Next :=
                       new Node'(The_Domain => From_Index.The_Domain,
                                 The_Range => From_Index.The_Range,
                                 Next => null);
                    To_Index := To_Index.Next;
                    From_Index := From_Index.Next;
                end loop;
            end if;
        end loop;
        To_The_Map.The_Cache.Is_Current := False;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Map : in out Map) is
    begin
        The_Map.The_Items := Items'(others => null);
        The_Map.The_Cache.Is_Current := False;
    end Clear;

    procedure Bind (The_Domain : in Domain;
                    And_The_Range : in Ranges;
                    In_The_Map : in out Map) is
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        Find (The_Domain, In_The_Map, The_Bucket, Previous_Node, Current_Node);
        if Current_Node /= null then
            raise Multiple_Binding;
        else
            In_The_Map.The_Items (The_Bucket) :=
               new Node'(The_Domain => The_Domain,
                         The_Range => And_The_Range,
                         Next => In_The_Map.The_Items (The_Bucket));
            In_The_Map.The_Cache := Cache'(The_Domain => The_Domain,
                                           The_Range => And_The_Range,
                                           Is_Bound => True,
                                           Is_Current => True);
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Bind;

    procedure Unbind (The_Domain : in Domain; In_The_Map : in out Map) is
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        Find (The_Domain, In_The_Map, The_Bucket, Previous_Node, Current_Node);
        if Previous_Node = null then
            In_The_Map.The_Items (The_Bucket) := Current_Node.Next;
        else
            Previous_Node.Next := Current_Node.Next;
        end if;
        In_The_Map.The_Cache.The_Domain := The_Domain;
        In_The_Map.The_Cache.Is_Bound := False;
        In_The_Map.The_Cache.Is_Current := True;
    exception
        when Constraint_Error =>
            raise Domain_Is_Not_Bound;
    end Unbind;

    function Is_Equal (Left : in Map; Right : in Map) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
        Left_Count : Natural;
        Right_Count : Natural;
    begin
        for Index in Left.The_Items'Range loop
            if (Left.The_Items (Index) = null) xor
               (Right.The_Items (Index) = null) then
                return False;
            else
                Left_Index := Left.The_Items (Index);
                Left_Count := 0;
                while Left_Index /= null loop
                    Right_Index := Right.The_Items (Index);
                    while Right_Index /= null loop
                        if (Left_Index.The_Domain = Right_Index.The_Domain) then
                            exit;
                        else
                            Right_Index := Right_Index.Next;
                        end if;
                    end loop;
                    if Left_Index.The_Range /= Right_Index.The_Range then
                        return False;
                    else
                        Left_Index := Left_Index.Next;
                        Left_Count := Left_Count + 1;
                    end if;
                end loop;
                Right_Index := Right.The_Items (Index);
                Right_Count := 0;
                while Right_Index /= null loop
                    Right_Index := Right_Index.Next;
                    Right_Count := Right_Count + 1;
                end loop;
                if Left_Count /= Right_Count then
                    return False;
                end if;
            end if;
        end loop;
        return True;
    exception
        when Constraint_Error =>
            return False;
    end Is_Equal;

    function Extent_Of (The_Map : in Map) return Natural is
        Count : Natural := 0;
        Temporary_Node : Structure;
    begin
        for Index in The_Map.The_Items'Range loop
            Temporary_Node := The_Map.The_Items (Index);
            while Temporary_Node /= null loop
                Count := Count + 1;
                Temporary_Node := Temporary_Node.Next;
            end loop;
        end loop;
        return Count;
    end Extent_Of;

    function Is_Empty (The_Map : in Map) return Boolean is
    begin
        if The_Map.The_Cache.Is_Bound then
            return False;
        else
            return (The_Map.The_Items = Items'(others => null));
        end if;
    end Is_Empty;

    function Is_Bound
                (The_Domain : in Domain; In_The_Map : in Map) return Boolean is
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        if In_The_Map.The_Cache.Is_Current and then
           In_The_Map.The_Cache.The_Domain = The_Domain then
            return In_The_Map.The_Cache.Is_Bound;
        else
            Find (The_Domain, In_The_Map, The_Bucket,
                  Previous_Node, Current_Node);
            return (Current_Node /= null);
        end if;
    end Is_Bound;

    function Range_Of
                (The_Domain : in Domain; In_The_Map : in Map) return Ranges is
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        if In_The_Map.The_Cache.Is_Current and then
           In_The_Map.The_Cache.The_Domain = The_Domain then
            if In_The_Map.The_Cache.Is_Bound then
                return In_The_Map.The_Cache.The_Range;
            else
                raise Domain_Is_Not_Bound;
            end if;
        else
            Find (The_Domain, In_The_Map, The_Bucket,
                  Previous_Node, Current_Node);
            return Current_Node.The_Range;
        end if;
    exception
        when Constraint_Error =>
            raise Domain_Is_Not_Bound;
    end Range_Of;

end Map_Simple_Cached_Sequential_Unbounded_Unmanaged_Noniterator;
