--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

generic
    type Item is private;
package Tree_Binary_Double_Unbounded_Controlled is

    type Tree is private;

    type Child is (Left, Right);

    Null_Tree : constant Tree;

    procedure Copy       (From_The_Tree : in     Tree; 
                          To_The_Tree   : in out Tree);
    procedure Clear      (The_Tree      : in out Tree);
    procedure Construct  (The_Item      : in     Item;
                          And_The_Tree  : in out Tree;
                          On_The_Child  : in     Child);
    procedure Set_Item   (Of_The_Tree   : in out Tree; 
                          To_The_Item   : in     Item);
    procedure Swap_Child (The_Child     : in     Child;
                          Of_The_Tree   : in out Tree;
                          And_The_Tree  : in out Tree);

    function Is_Equal  (Left      : in Tree; 
                        Right     : in Tree)  return Boolean;
    function Is_Null   (The_Tree  : in Tree)  return Boolean;
    function Item_Of   (The_Tree  : in Tree)  return Item;
    function Child_Of  (The_Tree  : in Tree; 
                        The_Child : in Child) return Tree;
    function Parent_Of (The_Tree  : in Tree)  return Tree;

    Overflow     : exception;
    Tree_Is_Null : exception;
    Not_At_Root  : exception;

private
    type Node;
    type Tree is access Node;
    Null_Tree : constant Tree := null;
end Tree_Binary_Double_Unbounded_Controlled;

--              Original Booch Components (Ada 83 version)
--                               
-- Copyright (C) 2000 Grady Booch
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
