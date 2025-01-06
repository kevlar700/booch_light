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
package body Tree_Binary_Double_Unbounded_Controlled is

    type Node is
        record
            Previous : Tree;
            The_Item : Item;
            Left_Subtree : Tree;
            Right_Subtree : Tree;
        end record;

    procedure Free (The_Node : in out Node) is
    begin
        The_Node.Previous := null;
        The_Node.Right_Subtree := null;
    end Free;

    procedure Set_Next (The_Node : in out Node; To_Next : in Tree) is
    begin
        The_Node.Left_Subtree := To_Next;
    end Set_Next;

    function Next_Of (The_Node : in Node) return Tree is
    begin
        return The_Node.Left_Subtree;
    end Next_Of;

    package Node_Manager is
       new Storage_Manager_Concurrent (Item => Node,
                                       Pointer => Tree,
                                       Free => Free,
                                       Set_Pointer => Set_Next,
                                       Pointer_Of => Next_Of);

    procedure Copy (From_The_Tree : in Tree; To_The_Tree : in out Tree) is
        Temporary_Node : Tree;
    begin
        Clear (To_The_Tree);
        if From_The_Tree = null then
            To_The_Tree := null;
        else
            To_The_Tree := Node_Manager.New_Item;
            To_The_Tree.The_Item := From_The_Tree.The_Item;
            Copy (From_The_Tree.Left_Subtree, To_The_Tree.Left_Subtree);
            if To_The_Tree.Left_Subtree /= null then
                To_The_Tree.Left_Subtree.Previous := To_The_Tree;
            end if;
            Copy (From_The_Tree.Right_Subtree, To_The_Tree.Right_Subtree);
            if To_The_Tree.Right_Subtree /= null then
                To_The_Tree.Right_Subtree.Previous := To_The_Tree;
            end if;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Tree : in out Tree) is
    begin
        if The_Tree /= null then
            Clear (The_Tree.Left_Subtree);
            Clear (The_Tree.Right_Subtree);
            The_Tree.Left_Subtree := null;
            The_Tree.Right_Subtree := null;
            Node_Manager.Free (The_Tree);
        end if;
    end Clear;

    procedure Construct (The_Item : in Item;
                         And_The_Tree : in out Tree;
                         On_The_Child : in Child) is
        Temporary_Node : Tree;
    begin
        if And_The_Tree = null then
            And_The_Tree := Node_Manager.New_Item;
            And_The_Tree.The_Item := The_Item;
        elsif And_The_Tree.Previous = null then
            Temporary_Node := Node_Manager.New_Item;
            Temporary_Node.The_Item := The_Item;
            if On_The_Child = Left then
                Temporary_Node.Left_Subtree := And_The_Tree;
            else
                Temporary_Node.Right_Subtree := And_The_Tree;
            end if;
            And_The_Tree.Previous := Temporary_Node;
            And_The_Tree := Temporary_Node;
        else
            raise Not_At_Root;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Construct;

    procedure Set_Item (Of_The_Tree : in out Tree; To_The_Item : in Item) is
    begin
        Of_The_Tree.The_Item := To_The_Item;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Set_Item;

    procedure Swap_Child (The_Child : in Child;
                          Of_The_Tree : in out Tree;
                          And_The_Tree : in out Tree) is
        Temporary_Node : Tree;
    begin
        if The_Child = Left then
            if And_The_Tree = null then
                if Of_The_Tree.Left_Subtree /= null then
                    Temporary_Node := Of_The_Tree.Left_Subtree;
                    Temporary_Node.Previous := null;
                    Of_The_Tree.Left_Subtree := null;
                    And_The_Tree := Temporary_Node;
                end if;
            elsif And_The_Tree.Previous = null then
                if Of_The_Tree.Left_Subtree /= null then
                    Temporary_Node := Of_The_Tree.Left_Subtree;
                    Temporary_Node.Previous := null;
                    Of_The_Tree.Left_Subtree := And_The_Tree;
                    And_The_Tree.Previous := Of_The_Tree;
                    And_The_Tree := Temporary_Node;
                else
                    And_The_Tree.Previous := Of_The_Tree;
                    Of_The_Tree.Left_Subtree := And_The_Tree;
                    And_The_Tree := null;
                end if;
            else
                raise Not_At_Root;
            end if;
        else
            if And_The_Tree = null then
                if Of_The_Tree.Right_Subtree /= null then
                    Temporary_Node := Of_The_Tree.Right_Subtree;
                    Temporary_Node.Previous := null;
                    Of_The_Tree.Right_Subtree := null;
                    And_The_Tree := Temporary_Node;
                end if;
            elsif And_The_Tree.Previous = null then
                if Of_The_Tree.Right_Subtree /= null then
                    Temporary_Node := Of_The_Tree.Right_Subtree;
                    Temporary_Node.Previous := null;
                    Of_The_Tree.Right_Subtree := And_The_Tree;
                    And_The_Tree.Previous := Of_The_Tree;
                    And_The_Tree := Temporary_Node;
                else
                    And_The_Tree.Previous := Of_The_Tree;
                    Of_The_Tree.Right_Subtree := And_The_Tree;
                    And_The_Tree := null;
                end if;
            else
                raise Not_At_Root;
            end if;
        end if;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Swap_Child;

    function Is_Equal (Left : in Tree; Right : in Tree) return Boolean is
    begin
        if Left.The_Item /= Right.The_Item then
            return False;
        else
            return (Is_Equal (Left.Left_Subtree, Right.Left_Subtree) and then
                    Is_Equal (Left.Right_Subtree, Right.Right_Subtree));
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

    function Child_Of (The_Tree : in Tree; The_Child : in Child) return Tree is
    begin
        if The_Child = Left then
            return The_Tree.Left_Subtree;
        else
            return The_Tree.Right_Subtree;
        end if;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Child_Of;

    function Parent_Of (The_Tree : in Tree) return Tree is
    begin
        return The_Tree.Previous;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Parent_Of;

end Tree_Binary_Double_Unbounded_Controlled;
