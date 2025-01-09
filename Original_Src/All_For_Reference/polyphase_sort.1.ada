--           Original Booch Components (Ada 83 version)
-- Copyright (C) 2000 Grady Booch, provided WITHOUT ANY WARRANTY.
-- Further license details should appear at the end of this file.

generic
    Number_Of_Files : in Positive;
    type Item is private;
    type File is limited private;
    with procedure Open_For_Reading (The_File : in out File);
    with procedure Open_For_Writing (The_File : in out File);
    with procedure Get              (The_File : in out File; 
                                     The_Item :    out Item);
    with procedure Put              (The_File : in out File; 
                                     The_Item : in     Item);
    with procedure Close            (The_File : in out File);

    with function Next_Item      (From_The_File : in File) return Item;
    with function "<"            (Left          : in Item; 
                                  Right         : in Item) return Boolean;
    with function Is_End_Of_File (The_File      : in File) return Boolean;
package Polyphase_Sort is

    type Files is array (1 .. Number_Of_Files) of File;

    procedure Sort (The_File        : in out File;
                    Temporary_Files : in out Files;
                    Sorted_File     :    out Positive);

    File_Is_Empty : exception;

end Polyphase_Sort;

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
