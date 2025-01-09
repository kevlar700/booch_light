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

with Map_Simple_Noncached_Concurrent_Unbounded_Managed_Iterator,
     Storage_Manager_Concurrent;
package body Tree_Arbitrary_Double_Unbounded_Controlled is

    function Hash_Of (The_Child : in Positive) return Positive;

    package Children is
       new Map_Simple_Noncached_Concurrent_Unbounded_Managed_Iterator
              (Domain => Positive,
               Ranges => Tree,
               Number_Of_Buckets => Expected_Number_Of_Children,
               Hash_Of => Hash_Of);

    type Node is
        record
            Previous : Tree;
            The_Item : Item;
            The_Children : Children.Map;
            Next : Tree;
        end record;

    function Hash_Of (The_Child : in Positive) return Positive is
    begin
        return The_Child;
    end Hash_Of;

    procedure Free (The_Node : in out Node) is
    begin
        The_Node.Previous := null;
        Children.Clear (The_Node.The_Children);
    end Free;

    procedure Set_Next (The_Node : in out Node; To_Next : in Tree) is
    begin
        The_Node.Next := To_Next;
    end Set_Next;

    function Next_Of (The_Node : in Node) return Tree is
    begin
        return The_Node.Next;
    end Next_Of;

    package Node_Manager is
       new Storage_Manager_Concurrent (Item => Node,
                                       Pointer => Tree,
                                       Free => Free,
                                       Set_Pointer => Set_Next,
                                       Pointer_Of => Next_Of);

    procedure Copy (From_The_Tree : in Tree; To_The_Tree : in out Tree) is
        procedure Copy_Child (The_Domain : in Positive;
                              The_Range : in Tree;
                              Continue : out Boolean) is
            Temporary_Node : Tree;
        begin  
            Copy (The_Range, To_The_Tree => Temporary_Node);
            Children.Bind (The_Domain, Temporary_Node,
                           In_The_Map => To_The_Tree.The_Children);
            if Temporary_Node /= Null_Tree then
                Temporary_Node.Previous := To_The_Tree;
            end if;
            Continue := True;
        end Copy_Child;
        procedure Copy_Children is new Children.Iterate (Copy_Child);
    begin
        Clear (To_The_Tree);
        if From_The_Tree /= null then
            To_The_Tree := Node_Manager.New_Item;
            To_The_Tree.The_Item := From_The_Tree.The_Item;
            Copy_Children (From_The_Tree.The_Children);
        end if;
    exception
        when Storage_Error | Children.Overflow =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Tree : in out Tree) is
        procedure Clear_Child (The_Domain : in Positive;
                               The_Range : in Tree;
                               Continue : out Boolean) is
            Temporary_Node : Tree := The_Range;
        begin
            Clear (Temporary_Node);
            Continue := True;
        end Clear_Child;
        procedure Clear_Children is new Children.Iterate (Clear_Child);
    begin
        if The_Tree /= null then
            Clear_Children (The_Tree.The_Children);
            Node_Manager.Free (The_Tree);
        end if;
    end Clear;

    procedure Construct (The_Item : in Item;
                         And_The_Tree : in out Tree;
                         Number_Of_Children : in Natural;
                         On_The_Child : in Natural) is
        Temporary_Node : Tree;
    begin
        if Number_Of_Children = 0 then
            if And_The_Tree = null then
                And_The_Tree := Node_Manager.New_Item;
                And_The_Tree.The_Item := The_Item;
                return;
            else
                raise Tree_Is_Not_Null;
            end if;
        elsif On_The_Child > Number_Of_Children then
            raise Child_Error;
        elsif And_The_Tree = null then
            And_The_Tree := Node_Manager.New_Item;
            And_The_Tree.The_Item := The_Item;
            for Index in 1 .. Number_Of_Children loop
                Children.Bind (The_Domain => Index,
                               And_The_Range => null,
                               In_The_Map => And_The_Tree.The_Children);
            end loop;
        elsif And_The_Tree.Previous = null then
            Temporary_Node := Node_Manager.New_Item;
            Temporary_Node.The_Item := The_Item;
            for Index in 1 .. Number_Of_Children loop
                if Index = On_The_Child then
                    Children.Bind (The_Domain => Index,
                                   And_The_Range => And_The_Tree,
                                   In_The_Map => Temporary_Node.The_Children);
                else
                    Children.Bind (The_Domain => Index,
                                   And_The_Range => null,
                                   In_The_Map => Temporary_Node.The_Children);
                end if;
            end loop;
            And_The_Tree.Previous := Temporary_Node;
            And_The_Tree := Temporary_Node;
        else
            raise Not_At_Root;
        end if;
    exception
        when Storage_Error | Children.Overflow =>
            raise Overflow;
    end Construct;

    procedure Set_Item (Of_The_Tree : in out Tree; To_The_Item : in Item) is
    begin
        Of_The_Tree.The_Item := To_The_Item;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Set_Item;

    procedure Swap_Child (The_Child : in Positive;
                          Of_The_Tree : in out Tree;
                          And_The_Tree : in out Tree) is
        Temporary_Node : Tree;
    begin
        if And_The_Tree = null then
            Temporary_Node := Children.Range_Of
                                 (The_Domain => The_Child,
                                  In_The_Map => Of_The_Tree.The_Children);
            Children.Unbind (The_Child, Of_The_Tree.The_Children);
            Children.Bind (The_Domain => The_Child,
                           And_The_Range => null,
                           In_The_Map => Of_The_Tree.The_Children);
            if Temporary_Node /= null then
                Temporary_Node.Previous := null;
            end if;
            And_The_Tree := Temporary_Node;
        elsif And_The_Tree.Previous = null then
            Temporary_Node := Children.Range_Of
                                 (The_Domain => The_Child,
                                  In_The_Map => Of_The_Tree.The_Children);
            Children.Unbind (The_Child, Of_The_Tree.The_Children);
            Children.Bind (The_Domain => The_Child,
                           And_The_Range => And_The_Tree,
                           In_The_Map => Of_The_Tree.The_Children);
            if Temporary_Node /= null then
                Temporary_Node.Previous := null;
            end if;
            And_The_Tree.Previous := Of_The_Tree;
            And_The_Tree := Temporary_Node;
        else
            raise Not_At_Root;
        end if;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
        when Children.Domain_Is_Not_Bound =>
            raise Child_Error;
    end Swap_Child;

    function Is_Equal (Left : in Tree; Right : in Tree) return Boolean is
        Trees_Are_Equal : Boolean := True;
        procedure Check_Child_Equality (The_Domain : in Positive;
                                        The_Range : in Tree;
                                        Continue : out Boolean) is
        begin
            if not Is_Equal (The_Range,
                             Children.Range_Of (The_Domain,
                                                Right.The_Children)) then
                Trees_Are_Equal := False;
                Continue := False;
            else
                Continue := True;
            end if;
        end Check_Child_Equality;
        procedure Check_Equality is new Children.Iterate (Check_Child_Equality);
    begin
        if Left.The_Item /= Right.The_Item then
            return False;
        else
            if Children.Extent_Of (Left.The_Children) /=
               Children.Extent_Of (Right.The_Children) then
                return False;
            else
                Check_Equality (Left.The_Children);
                return Trees_Are_Equal;
            end if;
        end if;
    exception
        when Constraint_Error =>
            return (Left = Null_Tree) and (Right = Null_Tree);
    end Is_Equal;

    function Is_Null (The_Tree : in Tree) return Boolean is
    begin
        return (The_Tree = null);
    end Is_Null;

    function Item_Of (The_Tree : in Tree) return Item is
    begin
        return The_Tree.The_Item;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Item_Of;

    function Number_Of_Children_In (The_Tree : in Tree) return Natural is
    begin
        return Children.Extent_Of (The_Tree.The_Children);
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Number_Of_Children_In;

    function Child_Of
                (The_Tree : in Tree; The_Child : in Positive) return Tree is
    begin
        return Children.Range_Of (The_Domain => The_Child,
                                  In_The_Map => The_Tree.The_Children);
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
        when Children.Domain_Is_Not_Bound =>
            raise Child_Error;
    end Child_Of;

    function Parent_Of (The_Tree : in Tree) return Tree is
    begin
        return The_Tree.Previous;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Parent_Of;

end Tree_Arbitrary_Double_Unbounded_Controlled;
