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

package body Tree_Utilities_Arbitrary_Double is

    function Is_Root (The_Tree : in Tree) return Boolean is
    begin
        return Is_Null (Parent_Of (The_Tree));
    end Is_Root;

    function Is_Leaf (The_Tree : in Tree) return Boolean is
    begin
        for Index in 1 .. Number_Of_Children_In (The_Tree) loop
            if not Is_Null (Child_Of (The_Tree, The_Child => Index)) then
                return False;
            end if;
        end loop;
        return True;
    end Is_Leaf;

    function Root_Of (The_Tree : in Tree) return Tree is
        Result : Tree := The_Tree;
    begin
        while not Is_Root (Result) loop
            Result := Parent_Of (Result);
        end loop;
        return Result;
    end Root_Of;

    function Child_Name_Of (The_Tree : in Tree) return Positive is
    begin
        if Is_Root (The_Tree) then
            raise Tree_Is_Root;
        else
            for Index in 1 .. Number_Of_Children_In (Parent_Of (The_Tree)) loop
                if Child_Of (Parent_Of (The_Tree), The_Child => Index) =
                   The_Tree then
                    return Index;
                end if;
            end loop;
        end if;
    end Child_Name_Of;

    function Number_Of_Siblings_Of (The_Tree : in Tree) return Natural is
    begin
        if Is_Root (The_Tree) then
            raise Tree_Is_Root;
        else
            return (Number_Of_Children_In (Parent_Of (The_Tree)) - 1);
        end if;
    end Number_Of_Siblings_Of;

    function Left_Sibling_Of (The_Tree : in Tree) return Tree is
    begin
        if Child_Name_Of (The_Tree) = 1 then
            return Null_Tree;
        else
            return Child_Of (Parent_Of (The_Tree),
                             The_Child => (Child_Name_Of (The_Tree) - 1));
        end if;
    end Left_Sibling_Of;

    function Right_Sibling_Of (The_Tree : in Tree) return Tree is
    begin
        if Child_Name_Of (The_Tree) =
           Number_Of_Children_In (Parent_Of (The_Tree)) then
            return Null_Tree;
        else
            return Child_Of (Parent_Of (The_Tree),
                             The_Child => (Child_Name_Of (The_Tree) + 1));
        end if;
    end Right_Sibling_Of;

    function Leftmost_Sibling_Of (The_Tree : in Tree) return Tree is
    begin
        if Child_Name_Of (The_Tree) = 1 then
            return Null_Tree;
        else
            return (Child_Of (Parent_Of (The_Tree), The_Child => 1));
        end if;
    end Leftmost_Sibling_Of;

    function Rightmost_Sibling_Of (The_Tree : in Tree) return Tree is
    begin
        if Child_Name_Of (The_Tree) =
           Number_Of_Children_In (Parent_Of (The_Tree)) then
            return Null_Tree;
        else
            return (Child_Of (Parent_Of (The_Tree),
                              The_Child => (Number_Of_Children_In
                                               (Parent_Of (The_Tree)))));
        end if;
    end Rightmost_Sibling_Of;

end Tree_Utilities_Arbitrary_Double;
