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

package body List_Utilities_Single is

    function Location_Of (The_Position : in Positive; In_The_List : in List) 
                         return List is
        Index : List;
    begin
        if Is_Null (In_The_List) then
            raise Position_Error;
        else
            Index := In_The_List;
            for Count in 2 .. The_Position loop
                Index := Tail_Of (Index);
                if Is_Null (Index) then
                    raise Position_Error;
                end if;
            end loop;
            return Index;
        end if;
    end Location_Of;

    procedure Construct (The_Items : in Items; And_The_List : in out List) is
    begin
        for Index in reverse The_Items'Range loop
            Construct (The_Items (Index), And_The_List);
        end loop;
    end Construct;

    procedure Construct (The_List : in out List; And_The_List : in out List) is
        Index : List := Foot_Of (The_List);
        Temporary_List : List := And_The_List;
    begin
        Swap_Tail (Index, Temporary_List);
    end Construct;

    procedure Split (The_List : in out List; 
                     At_The_Position : in Positive; 
                     Into_The_List : in out List) is
        Index : List;
    begin
        Index := Location_Of ((At_The_Position - 1), The_List);
        Clear (Into_The_List);
        Swap_Tail (Index, Into_The_List);
    exception
        when Constraint_Error =>
            raise Position_Error;
    end Split;

    procedure Insert (The_Item : in Item; 
                      In_The_List : in out List; 
                      After_The_Position : in Positive) is
        Index : List := Location_Of (After_The_Position, In_The_List);
        Temporary_List : List;
    begin
        Construct (The_Item, And_The_List => Temporary_List);
        Swap_Tail (Index, And_The_List => Temporary_List);
        Index := Tail_Of (Index);
        Swap_Tail (Index, And_The_List => Temporary_List);
    end Insert;

    procedure Insert (The_List : in out List; 
                      In_The_List : in out List; 
                      After_The_Position : in Positive) is
        Index : List := Location_Of (After_The_Position, In_The_List);
        Temporary_List : List := The_List;
        Temporary_Tail : List := Foot_Of (The_List);
    begin
        Swap_Tail (Index, And_The_List => Temporary_List);
        Swap_Tail (Temporary_Tail, And_The_List => Temporary_List);
    end Insert;

    procedure Insert (The_Item : in Item; After_The_List : in out List) is
        Index : List := Foot_Of (After_The_List);
        Temporary_List : List;
    begin
        Construct (The_Item, And_The_List => Temporary_List);
        Swap_Tail (Index, And_The_List => Temporary_List);
    end Insert;

    procedure Insert (The_List : in List; After_The_List : in out List) is
        Index : List := Foot_Of (After_The_List);
        Temporary_List : List := The_List;
    begin
        Swap_Tail (Index, And_The_List => Temporary_List);
    end Insert;

    procedure Remove_Item (In_The_List : in out List; 
                           At_The_Position : in Positive) is
        Index : List := Location_Of (At_The_Position, In_The_List);
        Temporary_List : List;
    begin
        if Index = In_The_List then
            Swap_Tail (Index, And_The_List => Temporary_List);
            Clear (Index);
            In_The_List := Temporary_List;
        else
            Swap_Tail (Index, And_The_List => Temporary_List);
            Index := Location_Of ((At_The_Position - 1), In_The_List);
            Swap_Tail (Index, And_The_List => Temporary_List);
            Clear (Temporary_List);
        end if;
    end Remove_Item;

    function Foot_Of (The_List : in List) return List is
        Index : List := The_List;
    begin
        while not Is_Null (Tail_Of (Index)) loop
            Index := Tail_Of (Index);
        end loop;
        return Index;
    end Foot_Of;

end List_Utilities_Single;
