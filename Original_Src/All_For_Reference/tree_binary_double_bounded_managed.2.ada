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

package body Tree_Binary_Double_Bounded_Managed is

    type Node is
        record
            Previous : Tree;
            The_Item : Item;
            Left_Subtree : Tree;
            Right_Subtree : Tree;
        end record;

    Heap : array (Positive range 1 .. The_Size) of Node;

    Free_List : Tree;

    procedure Free (The_Tree : in out Tree) is
        Temporary_Node : Tree;
    begin
        if The_Tree.The_Head /= 0 then
            Free (Heap (The_Tree.The_Head).Left_Subtree);
            Free (Heap (The_Tree.The_Head).Right_Subtree);
            Heap (The_Tree.The_Head).Previous := Null_Tree;
            Heap (The_Tree.The_Head).Left_Subtree := Free_List;
            Heap (The_Tree.The_Head).Right_Subtree := Null_Tree;
            Free_List := The_Tree;
            The_Tree := Null_Tree;
        end if;
    end Free;

    function New_Item return Tree is
        Temporary_Node : Tree;
    begin
        if Free_List.The_Head = 0 then
            raise Storage_Error;
        else
            Temporary_Node := Free_List;
            Free_List := Heap (Free_List.The_Head).Left_Subtree;
            Heap (Temporary_Node.The_Head).Left_Subtree := Null_Tree;
            return Temporary_Node;
        end if;
    end New_Item;

    procedure Copy (From_The_Tree : in Tree; To_The_Tree : in out Tree) is
    begin
        Free (To_The_Tree);
        if From_The_Tree = Null_Tree then
            To_The_Tree := Null_Tree;
        else
            To_The_Tree := New_Item;
            Heap (To_The_Tree.The_Head).The_Item :=
               Heap (From_The_Tree.The_Head).The_Item;
            Copy (Heap (From_The_Tree.The_Head).Left_Subtree,
                  Heap (To_The_Tree.The_Head).Left_Subtree);
            if Heap (To_The_Tree.The_Head).Left_Subtree /= Null_Tree then
                Heap (Heap (To_The_Tree.The_Head).Left_Subtree.The_Head).
                   Previous := To_The_Tree;
            end if;
            Copy (Heap (From_The_Tree.The_Head).Right_Subtree,
                  Heap (To_The_Tree.The_Head).Right_Subtree);
            if Heap (To_The_Tree.The_Head).Right_Subtree /= Null_Tree then
                Heap (Heap (To_The_Tree.The_Head).Right_Subtree.The_Head).
                   Previous := To_The_Tree;
            end if;
        end if;
    exception
        when Storage_Error =>
            raise Overflow;
    end Copy;

    procedure Clear (The_Tree : in out Tree) is
    begin
        Free (The_Tree);
    end Clear;

    procedure Construct (The_Item : in Item;
                         And_The_Tree : in out Tree;
                         On_The_Child : in Child) is
        Temporary_Node : Tree;
    begin
        if And_The_Tree = Null_Tree then
            And_The_Tree := New_Item;
            Heap (And_The_Tree.The_Head).The_Item := The_Item;
        elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
            Temporary_Node := New_Item;
            Heap (Temporary_Node.The_Head).The_Item := The_Item;
            Heap (And_The_Tree.The_Head).Previous := Temporary_Node;
            if On_The_Child = Left then
                Heap (Temporary_Node.The_Head).Left_Subtree := And_The_Tree;
            else
                Heap (Temporary_Node.The_Head).Right_Subtree := And_The_Tree;
            end if;
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
        Heap (Of_The_Tree.The_Head).The_Item := To_The_Item;
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
            if And_The_Tree = Null_Tree then
                if Heap (Of_The_Tree.The_Head).Left_Subtree /= Null_Tree then
                    Temporary_Node := Heap (Of_The_Tree.The_Head).Left_Subtree;
                    Heap (Temporary_Node.The_Head).Previous := Null_Tree;
                    Heap (Of_The_Tree.The_Head).Left_Subtree := Null_Tree;
                    And_The_Tree := Temporary_Node;
                end if;
            elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
                if Heap (Of_The_Tree.The_Head).Left_Subtree /= Null_Tree then
                    Temporary_Node := Heap (Of_The_Tree.The_Head).Left_Subtree;
                    Heap (Temporary_Node.The_Head).Previous := Null_Tree;
                    Heap (Of_The_Tree.The_Head).Left_Subtree := And_The_Tree;
                    Heap (And_The_Tree.The_Head).Previous := Of_The_Tree;
                    And_The_Tree := Temporary_Node;
                else
                    Heap (And_The_Tree.The_Head).Previous := Of_The_Tree;
                    Heap (Of_The_Tree.The_Head).Left_Subtree := And_The_Tree;
                    And_The_Tree := Null_Tree;
                end if;
            else
                raise Not_At_Root;
            end if;
        else
            if And_The_Tree = Null_Tree then
                if Heap (Of_The_Tree.The_Head).Right_Subtree /= Null_Tree then
                    Temporary_Node := Heap (Of_The_Tree.The_Head).Right_Subtree;
                    Heap (Temporary_Node.The_Head).Previous := Null_Tree;
                    Heap (Of_The_Tree.The_Head).Right_Subtree := Null_Tree;
                    And_The_Tree := Temporary_Node;
                end if;
            elsif Heap (And_The_Tree.The_Head).Previous = Null_Tree then
                if Heap (Of_The_Tree.The_Head).Right_Subtree /= Null_Tree then
                    Temporary_Node := Heap (Of_The_Tree.The_Head).Right_Subtree;
                    Heap (Temporary_Node.The_Head).Previous := Null_Tree;
                    Heap (Of_The_Tree.The_Head).Right_Subtree := And_The_Tree;
                    Heap (And_The_Tree.The_Head).Previous := Of_The_Tree;
                    And_The_Tree := Temporary_Node;
                else
                    Heap (And_The_Tree.The_Head).Previous := Of_The_Tree;
                    Heap (Of_The_Tree.The_Head).Right_Subtree := And_The_Tree;
                    And_The_Tree := Null_Tree;
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
        if Heap (Left.The_Head).The_Item /= Heap (Right.The_Head).The_Item then
            return False;
        else
            return (Is_Equal (Heap (Left.The_Head).Left_Subtree,
                              Heap (Right.The_Head).Left_Subtree) and then
                    Is_Equal (Heap (Left.The_Head).Right_Subtree,
                              Heap (Right.The_Head).Right_Subtree));
        end if;
    exception
        when Constraint_Error =>
            return (Left = Null_Tree) and (Right = Null_Tree);
    end Is_Equal;

    function Is_Null (The_Tree : in Tree) return Boolean is
    begin
        return (The_Tree = Null_Tree);
    end Is_Null;

    function Item_Of (The_Tree : in Tree) return Item is
    begin
        return Heap (The_Tree.The_Head).The_Item;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Item_Of;

    function Child_Of (The_Tree : in Tree; The_Child : in Child) return Tree is
    begin
        if The_Child = Left then
            return Heap (The_Tree.The_Head).Left_Subtree;
        else
            return Heap (The_Tree.The_Head).Right_Subtree;
        end if;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Child_Of;

    function Parent_Of (The_Tree : in Tree) return Tree is
    begin
        return Heap (The_Tree.The_Head).Previous;
    exception
        when Constraint_Error =>
            raise Tree_Is_Null;
    end Parent_Of;

begin
    Free_List.The_Head := 1;
    for Index in 1 .. (The_Size - 1) loop
        Heap (Index).Left_Subtree := Tree'(The_Head => (Index + 1));
    end loop;
    Heap (The_Size).Left_Subtree := Null_Tree;
end Tree_Binary_Double_Bounded_Managed;
