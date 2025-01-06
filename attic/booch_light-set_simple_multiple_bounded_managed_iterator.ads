--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Monitor;
generic
    type Item is private;
package Booch_Light.Set_Simple_Multiple_Bounded_Managed_Iterator is

    type Set (The_Size : Positive) is limited private;

    procedure Copy         (From_The_Set : in     Set; 
                            To_The_Set   : in out Set);                    
    procedure Clear        (The_Set      : in out Set);
    procedure Add          (The_Item     : in     Item; 
                            To_The_Set   : in out Set);
    procedure Remove       (The_Item     : in     Item; 
                            From_The_Set : in out Set);                    
    procedure Union        (Of_The_Set   : in     Set;
                            And_The_Set  : in     Set;
                            To_The_Set   : in out Set);
    procedure Intersection (Of_The_Set   : in     Set;
                            And_The_Set  : in     Set;
                            To_The_Set   : in out Set);
    procedure Difference   (Of_The_Set   : in     Set;
                            And_The_Set  : in     Set;
                            To_The_Set   : in out Set);

    function Is_Equal           (Left       : in Set; 
                                 Right      : in Set) return Boolean;                        
    function Extent_Of          (The_Set    : in Set) return Natural;
    function Is_Empty           (The_Set    : in Set) return Boolean;
    function Is_A_Member        (The_Item   : in Item; 
                                 Of_The_Set : in Set) return Boolean;                 
    function Is_A_Subset        (Left       : in Set; 
                                 Right      : in Set) return Boolean;
    function Is_A_Proper_Subset (Left       : in Set; 
                                 Right      : in Set) return Boolean;

    generic
        with procedure Process (The_Item : in  Item; 
                                Continue : out Boolean);
    procedure Iterate (Over_The_Set : in Set);

    Overflow           : exception;
    Item_Is_In_Set     : exception;
    Item_Is_Not_In_Set : exception;

private
    type Items is array (Positive range <>) of Item;
    type Set (The_Size : Positive) is
        record
            Guard     : Monitor.Kind;
            The_Back  : Natural := 0;
            The_Items : Items (1 .. The_Size);
        end record;
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
