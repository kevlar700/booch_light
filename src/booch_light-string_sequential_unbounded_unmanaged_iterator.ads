--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

generic
   type Item is private;
   type Substring is array (Positive range <>) of Item;
   with function "<"
     (Left  : in Item;
      Right : in Item)
      return Boolean;
package Booch_Light.String_Sequential_Unbounded_Unmanaged_Iterator is
   --
   --   Instantiation_Example
   --
   --   package Character_String is new
   --      Booch_Light.Unbounded_String
   --         (Item => Standard.Character,
   --         Substring => Standard.String,
   --         "<" => "<");
   --

   package Locus is

      subtype Copy is Status_Code with
          Static_Predicate => Copy in Exception_Storage_Error | OK;

      subtype Prepend is Status_Code with
          Static_Predicate => Prepend in Exception_Storage_Error | OK;

      subtype Append is Status_Code with
          Static_Predicate => Append in Exception_Storage_Error | OK;

      subtype Insert is Status_Code with
          Static_Predicate =>
           Insert in Position_Error | Exception_Storage_Error | OK;

      subtype Delete is Status_Code with
          Static_Predicate => Delete in Position_Error | OK;

      subtype Substring_Of is Status_Code with
          Static_Predicate =>
           Substring_Of in Position_Error | Exception_Constraint_Error | OK;

      subtype Replace is Status_Code with
          Static_Predicate => Replace in Position_Error | OK;

      subtype Set_Item is Status_Code with
          Static_Predicate => Set_Item in Position_Error | OK;

      subtype Item_Of is Status_Code with
          Static_Predicate => Item_Of in Position_Error | OK;

   end Locus;

   type U_String is limited private;

   procedure Copy
     (From_The_String : in     U_String;
      To_The_String   : in out U_String;
      Booch_Status    :    out Locus.Copy);

   procedure Copy
     (From_The_Substring : in     Substring;
      To_The_String      : in out U_String;
      Booch_Status       :    out Locus.Copy);

   procedure Clear (The_String : in out U_String);

   procedure Prepend
     (The_String    : in     U_String;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Prepend);

   procedure Prepend
     (The_Substring : in     Substring;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Prepend);

   procedure Append
     (The_String    : in     U_String;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Append);

   procedure Append
     (The_Substring : in     Substring;
      To_The_String : in out U_String;
      Booch_Status  :    out Locus.Append);

   procedure Insert
     (The_String      : in     U_String;
      In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      Booch_Status    :    out Locus.Insert);

   procedure Insert
     (The_Substring   : in     Substring;
      In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      Booch_Status    :    out Locus.Insert);

   procedure Delete
     (In_The_String     : in out U_String;
      From_The_Position : in     Positive;
      To_The_Position   : in     Positive;
      Booch_Status      :    out Locus.Delete);

   procedure Replace
     (In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      With_The_String : in     U_String;
      Booch_Status    :    out Locus.Replace);

   procedure Replace
     (In_The_String      : in out U_String;
      At_The_Position    : in     Positive;
      With_The_Substring : in     Substring;
      Booch_Status       :    out Locus.Replace);

   procedure Set_Item
     (In_The_String   : in out U_String;
      At_The_Position : in     Positive;
      With_The_Item   : in     Item;
      Booch_Status    :    out Locus.Set_Item);

   function Is_Equal
     (Left  : in U_String;
      Right : in U_String)
      return Boolean;

   function Is_Equal
     (Left  : in Substring;
      Right : in U_String)
      return Boolean;

   function Is_Equal
     (Left  : in U_String;
      Right : in Substring)
      return Boolean;

   function Is_Less_Than
     (Left  : in U_String;
      Right : in U_String)
      return Boolean;

   function Is_Less_Than
     (Left  : in Substring;
      Right : in U_String)
      return Boolean;

   function Is_Less_Than
     (Left  : in U_String;
      Right : in Substring)
      return Boolean;

   function Is_Greater_Than
     (Left  : in U_String;
      Right : in U_String)
      return Boolean;

   function Is_Greater_Than
     (Left  : in Substring;
      Right : in U_String)
      return Boolean;

   function Is_Greater_Than
     (Left  : in U_String;
      Right : in Substring)
      return Boolean;

   function Length_Of
     (The_String : in U_String)
      return Natural;

   function Is_Null
     (The_String : in U_String)
      return Boolean;

   procedure Item_Of
     (The_String      : in     U_String;
      At_The_Position : in     Positive;
      The_Item        :    out Item;
      Booch_Status    :    out Locus.Item_Of);

   --  function Substring_Of
   --    (The_String : in U_String)
   --     return Substring;
   --
   --  function Substring_Of
   --    (The_String        : in U_String;
   --     From_The_Position : in Positive;
   --     To_The_Position   : in Positive)
   --     return Substring;

   function Substring_Of_Length
     (The_String : in U_String)
      return Natural;

   procedure Substring_Of
     (The_String    : in     U_String;
      The_Substring : in out Substring;
      Booch_Status  :    out Locus.Substring_Of);

   function Substring_Of_Length
     (The_String        : in U_String;
      From_The_Position : in Positive;
      To_The_Position   : in Positive)
      return Natural;

   procedure Substring_Of
     (The_String        : in     U_String;
      From_The_Position : in     Positive;
      To_The_Position   : in     Positive;
      The_Substring     : in out Substring;
      Booch_Status      :    out Locus.Substring_Of);

   generic
      with procedure Process
        (The_Item : in     Item;
         Continue :    out Boolean);
   procedure Iterate (Over_The_String : in U_String);

private
   type Structure is access Substring;
   type U_String is record
      The_Length : Natural := 0;
      The_Items  : Structure;
   end record;

end Booch_Light.String_Sequential_Unbounded_Unmanaged_Iterator;

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
