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

with Storage_Manager_Concurrent;
package body Map_Simple_Cached_Concurrent_Unbounded_Managed_Iterator is

    type Node is
        record
            The_Domain : Domain;
            The_Range : Ranges;
            Next : Structure;
        end record;

    procedure Free (The_Node : in out Node) is
    begin
        null;
    end Free;

    procedure Set_Next (The_Node : in out Node; To_Next : in Structure) is
    begin
        The_Node.Next := To_Next;
    end Set_Next;

    function Next_Of (The_Node : in Node) return Structure is
    begin
        return The_Node.Next;
    end Next_Of;

    package Node_Manager is
       new Storage_Manager_Concurrent (Item => Node,
                                       Pointer => Structure,
                                       Free => Free,
                                       Set_Pointer => Set_Next,
                                       Pointer_Of => Next_Of);

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

    procedure Seize (The_Map : in Map) is
    begin
        Semaphore.Seize (The_Map.Guard);
    end Seize;

    procedure Release (The_Map : in Map) is
    begin
        Semaphore.Release (The_Map.Guard);
    end Release;

    procedure Copy (From_The_Map : in Map; To_The_Map : in out Map) is
        From_Index : Structure;
        To_Index : Structure;
    begin
        Seize (From_The_Map);
        Seize (To_The_Map);
        for Index in To_The_Map.The_Items'Range loop
            Node_Manager.Free (To_The_Map.The_Items (Index));
        end loop;
        for Index in From_The_Map.The_Items'Range loop
            From_Index := From_The_Map.The_Items (Index);
            if From_The_Map.The_Items (Index) /= null then
                To_The_Map.The_Items (Index) := Node_Manager.New_Item;
                To_The_Map.The_Items (Index).The_Domain :=
                   From_Index.The_Domain;
                To_The_Map.The_Items (Index).The_Range := From_Index.The_Range;
                To_Index := To_The_Map.The_Items (Index);
                From_Index := From_Index.Next;
                while From_Index /= null loop
                    To_Index.Next := Node_Manager.New_Item;
                    To_Index.Next.The_Domain := From_Index.The_Domain;
                    To_Index.Next.The_Range := From_Index.The_Range;
                    To_Index := To_Index.Next;
                    From_Index := From_Index.Next;
                end loop;
            end if;
        end loop;
        To_The_Map.The_Cache.Is_Current := False;
        Release (From_The_Map);
        Release (To_The_Map);
    exception
        when Storage_Error =>
            Release (From_The_Map);
            Release (To_The_Map);
            raise Overflow;
    end Copy;

    procedure Clear (The_Map : in out Map) is
    begin
        Seize (The_Map);
        for Index in The_Map.The_Items'Range loop
            Node_Manager.Free (The_Map.The_Items (Index));
        end loop;
        The_Map.The_Cache.Is_Current := False;
        Release (The_Map);
    end Clear;

    procedure Bind (The_Domain : in Domain;
                    And_The_Range : in Ranges;
                    In_The_Map : in out Map) is
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
        Temporary_Node : Structure;
    begin
        Seize (In_The_Map);
        Find (The_Domain, In_The_Map, The_Bucket, Previous_Node, Current_Node);
        if Current_Node /= null then
            Release (In_The_Map);
            raise Multiple_Binding;
        else
            Temporary_Node := Node_Manager.New_Item;
            Temporary_Node.The_Domain := The_Domain;
            Temporary_Node.The_Range := And_The_Range;
            Temporary_Node.Next := In_The_Map.The_Items (The_Bucket);
            In_The_Map.The_Items (The_Bucket) := Temporary_Node;
            In_The_Map.The_Cache := Cache'(The_Domain => The_Domain,
                                           The_Range => And_The_Range,
                                           Is_Bound => True,
                                           Is_Current => True);
        end if;
        Release (In_The_Map);
    exception
        when Storage_Error =>
            Release (In_The_Map);
            raise Overflow;
    end Bind;

    procedure Unbind (The_Domain : in Domain; In_The_Map : in out Map) is
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        Seize (In_The_Map);
        Find (The_Domain, In_The_Map, The_Bucket, Previous_Node, Current_Node);
        if Previous_Node = null then
            In_The_Map.The_Items (The_Bucket) := Current_Node.Next;
        else
            Previous_Node.Next := Current_Node.Next;
        end if;
        Current_Node.Next := null;
        Node_Manager.Free (Current_Node);
        In_The_Map.The_Cache.The_Domain := The_Domain;
        In_The_Map.The_Cache.Is_Bound := False;
        In_The_Map.The_Cache.Is_Current := True;
        Release (In_The_Map);
    exception
        when Constraint_Error =>
            Release (In_The_Map);
            raise Domain_Is_Not_Bound;
    end Unbind;

    function Is_Equal (Left : in Map; Right : in Map) return Boolean is
        Left_Index : Structure;
        Right_Index : Structure;
        Left_Count : Natural;
        Right_Count : Natural;
    begin
        Seize (Left);
        Seize (Right);
        for Index in Left.The_Items'Range loop
            if (Left.The_Items (Index) = null) xor
               (Right.The_Items (Index) = null) then
                Release (Left);
                Release (Right);
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
                        Release (Left);
                        Release (Right);
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
        Release (Left);
        Release (Right);
        return True;
    exception
        when Constraint_Error =>
            Release (Left);
            Release (Right);
            return False;
    end Is_Equal;

    function Extent_Of (The_Map : in Map) return Natural is
        Count : Natural := 0;
        Temporary_Node : Structure;
    begin
        Seize (The_Map);
        for Index in The_Map.The_Items'Range loop
            Temporary_Node := The_Map.The_Items (Index);
            while Temporary_Node /= null loop
                Count := Count + 1;
                Temporary_Node := Temporary_Node.Next;
            end loop;
        end loop;
        Release (The_Map);
        return Count;
    end Extent_Of;

    function Is_Empty (The_Map : in Map) return Boolean is
        Result : Boolean;
    begin
        Seize (The_Map);
        if The_Map.The_Cache.Is_Bound then
            Release (The_Map);
            return False;
        else
            Result := The_Map.The_Items = Items'(others => null);
            Release (The_Map);
            return Result;
        end if;
    end Is_Empty;

    function Is_Bound
                (The_Domain : in Domain; In_The_Map : in Map) return Boolean is
        Result : Boolean;
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        Seize (In_The_Map);
        if In_The_Map.The_Cache.Is_Current and then
           In_The_Map.The_Cache.The_Domain = The_Domain then
            Result := In_The_Map.The_Cache.Is_Bound;
            Release (In_The_Map);
            return Result;
        else
            Find (The_Domain, In_The_Map, The_Bucket,
                  Previous_Node, Current_Node);
            Release (In_The_Map);
            return (Current_Node /= null);
        end if;
    end Is_Bound;

    function Range_Of
                (The_Domain : in Domain; In_The_Map : in Map) return Ranges is
        Temporary_Item : Ranges;
        The_Bucket : Positive;
        Previous_Node : Structure;
        Current_Node : Structure;
    begin
        Seize (In_The_Map);
        if In_The_Map.The_Cache.Is_Current and then
           In_The_Map.The_Cache.The_Domain = The_Domain then
            if In_The_Map.The_Cache.Is_Bound then
                Temporary_Item := In_The_Map.The_Cache.The_Range;
                Release (In_The_Map);
                return Temporary_Item;
            else
                Release (In_The_Map);
                raise Domain_Is_Not_Bound;
            end if;
        else
            Find (The_Domain, In_The_Map, The_Bucket,
                  Previous_Node, Current_Node);
            Temporary_Item := Current_Node.The_Range;
            Release (In_The_Map);
            return Temporary_Item;
        end if;
    exception
        when Constraint_Error =>
            Release (In_The_Map);
            raise Domain_Is_Not_Bound;
    end Range_Of;

    procedure Iterate (Over_The_Map : in Map) is
        The_Bucket : Positive := Over_The_Map.The_Items'Last;
        The_Node : Structure;
        Continue : Boolean;
    begin
        Seize (Over_The_Map);
        for The_Iterator in Over_The_Map.The_Items'Range loop
            if Over_The_Map.The_Items (The_Iterator) /= null then
                The_Bucket := The_Iterator;
                The_Node := Over_The_Map.The_Items (The_Iterator);
                exit;
            end if;
        end loop;
        while The_Node /= null loop
            Process (The_Node.The_Domain, The_Node.The_Range, Continue);
            exit when not Continue;
            The_Node := The_Node.Next;
            if The_Node = null then
                for The_Iterator in (The_Bucket + 1) ..
                                       Over_The_Map.The_Items'Last loop
                    if Over_The_Map.The_Items (The_Iterator) /= null then
                        The_Bucket := The_Iterator;
                        The_Node := Over_The_Map.The_Items (The_Iterator);
                        exit;
                    end if;
                end loop;
            end if;
        end loop;
        Release (Over_The_Map);
    end Iterate;

end Map_Simple_Cached_Concurrent_Unbounded_Managed_Iterator;
