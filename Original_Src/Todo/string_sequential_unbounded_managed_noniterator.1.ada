--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

generic
    type Item is private;
    type Substring is array (Positive range <>) of Item;
    with function "<" (Left  : in Item; 
                       Right : in Item) return Boolean;
package String_Sequential_Unbounded_Managed_Noniterator is

    type String is limited private;

    procedure Copy     (From_The_String    : in     String; 
                        To_The_String      : in out String);
    procedure Copy     (From_The_Substring : in     Substring;
                        To_The_String      : in out String);
    procedure Clear    (The_String         : in out String);
    procedure Prepend  (The_String         : in     String; 
                        To_The_String      : in out String);
    procedure Prepend  (The_Substring      : in     Substring;
                        To_The_String      : in out String);
    procedure Append   (The_String         : in     String; 
                        To_The_String      : in out String);
    procedure Append   (The_Substring      : in     Substring;
                        To_The_String      : in out String);
    procedure Insert   (The_String         : in     String;
                        In_The_String      : in out String;
                        At_The_Position    : in     Positive);
    procedure Insert   (The_Substring      : in     Substring;
                        In_The_String      : in out String;
                        At_The_Position    : in     Positive);
    procedure Delete   (In_The_String      : in out String;
                        From_The_Position  : in     Positive;
                        To_The_Position    : in     Positive);
    procedure Replace  (In_The_String      : in out String;
                        At_The_Position    : in     Positive;
                        With_The_String    : in     String);
    procedure Replace  (In_The_String      : in out String;
                        At_The_Position    : in     Positive;
                        With_The_Substring : in     Substring);
    procedure Set_Item (In_The_String      : in out String;
                        At_The_Position    : in     Positive;
                        With_The_Item      : in     Item);

    function Is_Equal        (Left            : in String; 
                              Right           : in String)    return Boolean;
    function Is_Equal        (Left            : in Substring; 
                              Right           : in String)    return Boolean;
    function Is_Equal        (Left            : in String; 
                              Right           : in Substring) return Boolean;
    function Is_Less_Than    (Left            : in String;  
                              Right           : in String)    return Boolean;
    function Is_Less_Than    (Left            : in Substring; 
                              Right           : in String)    return Boolean;
    function Is_Less_Than    (Left            : in String; 
                              Right           : in Substring) return Boolean;
    function Is_Greater_Than (Left            : in String; 
                              Right           : in String)    return Boolean;
    function Is_Greater_Than (Left            : in Substring; 
                              Right           : in String)    return Boolean;
    function Is_Greater_Than (Left            : in String; 
                              Right           : in Substring) return Boolean;
    function Length_Of       (The_String      : in String)    return Natural;
    function Is_Null         (The_String      : in String)    return Boolean;
    function Item_Of         (The_String      : in String; 
                              At_The_Position : in Positive)  return Item;
    function Substring_Of    (The_String      : in String)    return Substring;
    function Substring_Of    (The_String      : in String;
                              From_The_Position:in Positive;
                              To_The_Position  :in Positive)  return Substring;

    Overflow       : exception;
    Position_Error : exception;

private
    type Structure is access Substring;
    type String is
        record
            The_Length : Natural := 0;
            The_Items  : Structure;
        end record;
end String_Sequential_Unbounded_Managed_Noniterator;

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
