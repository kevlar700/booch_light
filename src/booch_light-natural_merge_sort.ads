--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
   type File is limited private;
   with procedure Open_For_Reading (The_File : in out File);
   with procedure Open_For_Writing (The_File : in out File);

   with procedure Get
     (The_File : in out File;
      The_Item :    out Item);

   with procedure Put
     (The_File : in out File;
      The_Item :        Item);

   with procedure Close (The_File : in out File);

   with function Next_Item
     (The_File : File)
      return Item;

   with function "<"
     (Left  : Item;
      Right : Item)
      return Boolean;

   with function Is_End_Of_File
     (The_File : File)
      return Boolean;

package Booch_Light.Natural_Merge_Sort is

   package Locus is

      subtype Sort is Status_Code with
          Static_Predicate => Sort in File_Is_Empty | OK;

   end Locus;

   procedure Sort
     (The_File         : in out File;
      Temporary_File_1 : in out File;
      Temporary_File_2 : in out File;
      Booch_Status     :    out Locus.Sort);

end Booch_Light.Natural_Merge_Sort;

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
--  DEALINGS IN THE SOFTWARE. DEALINGS IN THE SOFTWARE. DEALINGS IN THE
--  SOFTWARE. DEALINGS IN THE SOFTWARE.
