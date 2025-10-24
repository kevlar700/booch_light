--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Tree_Utilities_Binary_Single is

   function Is_Leaf
     (The_Tree : Tree)
      return Boolean
   is
   begin
      return
        (Is_Null (Child_Of (The_Tree, Left_Child))
         and then Is_Null (Child_Of (The_Tree, Right_Child)));
   end Is_Leaf;

end Booch_Light.Tree_Utilities_Binary_Single;

--              Original Booch Components (Ada 83 version)
--  License: MIT
--  Copyright (C) 1987 Grady Booch Copyright (C) 2024 Kevin Chadwick (Light
--  runtime compatibility)
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of this software and associated documentation files (the
--  “Software”), to deal in the Software without restriction, including
--  without limitation the rights to use, copy, modify, merge, publish,
--  distribute, sublicense, and/or sell copies of the Software, and to permit
--  persons to whom the Software is furnished to do so, subject to- DEALINGS
--  IN THE SOFTWARE.
