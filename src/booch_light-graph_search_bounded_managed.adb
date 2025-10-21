--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;
with Booch_Light.Set_Simple_Sequential_Bounded_Managed_Noniterator;
with Booch_Light.Stack_Sequential_Bounded_Managed_Noniterator;
with
  Booch_Light
    .Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator;

package body Booch_Light.Graph_Search_Bounded_Managed is

   package Vertex_Set is new Set_Simple_Sequential_Bounded_Managed_Noniterator
     (Item => Vertex);

   package Vertex_Stack is new Stack_Sequential_Bounded_Managed_Noniterator
     (Item => Vertex);

   package Vertex_Queue is new Queue_Nonpriority_Nonbalking_Sequential_Bounded_Managed_Noniterator
     (Item => Vertex);

   procedure Traverse_Depth_First
     (From_The_Vertex : in     Vertex;
      In_The_Graph    : in     Graph;
      Booch_Status    :    out Locus.Traverse_Depth_First)
   is
      The_Size : constant Natural := Number_Of_Vertices_In (In_The_Graph);
      Vertices_Visited : Vertex_Set.Set (The_Size);
      Vertices_Ready   : Vertex_Stack.Stack (The_Size);
      Temporary_Vertex : Vertex;
      The_Iterator     : Iterator;
      Continue         : Boolean;
      Push_Status      : Vertex_Stack.Locus.Push;
      Add_Status       : Vertex_Set.Locus.Add;
   begin

      Vertex_Set.Add
        (The_Item     => From_The_Vertex,
         To_The_Set   => Vertices_Visited,
         Booch_Status => Add_Status);

      case Add_Status is
         when Item_Is_In_Set | Exception_Overflow | Exception_Storage_Error =>
            Booch_Status := Add_Status;
            Alogs.Log
              (Log_ID  => "FF27F5892D952670",
               Message => "Traverse_Depth_First failed");
            return;

         when OK =>
            null;
      end case;

      Vertex_Stack.Push
        (The_Item     => From_The_Vertex,
         On_The_Stack => Vertices_Ready,
         Booch_Status => Push_Status);

      case Push_Status is
         when Exception_Overflow =>
            Booch_Status := Push_Status;
            Alogs.Log
              (Log_ID  => "9E186851A8B9A6E3",
               Message => "Exception_Overflow: Traverse_Depth_First failed");
            return;

         when OK =>
            null;
      end case;

      while not Vertex_Stack.Is_Empty (Vertices_Ready) loop

         declare
            Top_Of_Status : Vertex_Stack.Locus.Top_Of;
         begin
            Vertex_Stack.Top_Of
              (The_Stack    => Vertices_Ready,
               The_Top      => Temporary_Vertex,
               Booch_Status => Top_Of_Status);

            case Top_Of_Status is
               when Exception_Underflow =>
                  Booch_Status := Top_Of_Status;
                  Alogs.Log
                    (Log_ID  => "B8F70481BE00D61E",
                     Message =>
                       "Exception_Underflow: Traverse_Depth_First failed");
                  return;

               when OK =>
                  null;
            end case;
         end;

         declare
            Pop_Status : Vertex_Stack.Locus.Pop;
         begin
            Vertex_Stack.Pop
              (The_Stack    => Vertices_Ready,
               Booch_Status => Pop_Status);

            case Pop_Status is
               when Exception_Underflow =>
                  Booch_Status := Pop_Status;
                  Alogs.Log
                    (Log_ID  => "CAA89A68A0A85A7C",
                     Message =>
                       "Exception_Underflow: Traverse_Depth_First failed");
                  return;

               when OK =>
                  null;
            end case;

         end;

         Process (Temporary_Vertex, Continue);
         exit when not Continue;
         Initialize
           (The_Iterator,
            With_The_Vertex => Temporary_Vertex);
         while not Is_Done (The_Iterator) loop
            if not Vertex_Set.Is_A_Member
                (Value_Of (The_Iterator), Vertices_Visited)
            then

               Vertex_Set.Add
                 (The_Item     => Value_Of (The_Iterator),
                  To_The_Set   => Vertices_Visited,
                  Booch_Status => Add_Status);

               case Add_Status is
                  when Item_Is_In_Set | Exception_Overflow
                    | Exception_Storage_Error =>
                     Booch_Status := Add_Status;
                     Alogs.Log
                       (Log_ID  => "DAE03E1E14A689D1",
                        Message => "Traverse_Depth_First failed");
                     return;

                  when OK =>
                     null;
               end case;

               Vertex_Stack.Push
                 (The_Item     => Value_Of (The_Iterator),
                  On_The_Stack => Vertices_Ready,
                  Booch_Status => Push_Status);

               case Push_Status is
                  when Exception_Overflow =>
                     Booch_Status := Push_Status;
                     Alogs.Log
                       (Log_ID  => "E93B5E8068FE7AEA",
                        Message =>
                          "Exception_Overflow: Traverse_Depth_First failed");
                     return;

                  when OK =>
                     null;
               end case;

            end if;
            Get_Next (The_Iterator);
         end loop;
      end loop;
   end Traverse_Depth_First;

   procedure Traverse_Breadth_First
     (From_The_Vertex : in     Vertex;
      In_The_Graph    : in     Graph;
      Booch_Status    :    out Locus.Traverse_Breadth_First)
   is
      The_Size : constant Natural := Number_Of_Vertices_In (In_The_Graph);
      Vertices_Visited : Vertex_Set.Set (The_Size);
      Vertices_Ready   : Vertex_Queue.Queue (The_Size);
      Temporary_Vertex : Vertex;
      The_Iterator     : Iterator;
      Continue         : Boolean;
      Add_Queue_Status : Vertex_Queue.Locus.Add;
      Add_Set_Status   : Vertex_Set.Locus.Add;
   begin

      Vertex_Set.Add
        (The_Item     => From_The_Vertex,
         To_The_Set   => Vertices_Visited,
         Booch_Status => Add_Set_Status);

      case Add_Set_Status is
         when Exception_Overflow | Exception_Storage_Error | Item_Is_In_Set =>
            Booch_Status := Add_Set_Status;
            Alogs.Log
              (Log_ID  => "2EC0E0CEC5FA1BFA",
               Message => "Traverse_Breadth_First failed");
            return;

         when OK =>
            null;
      end case;

      Vertex_Queue.Add
        (The_Item     => From_The_Vertex,
         To_The_Queue => Vertices_Ready,
         Booch_Status => Add_Queue_Status);

      case Add_Queue_Status is
         when Exception_Overflow =>
            Booch_Status := Add_Queue_Status;
            Alogs.Log
              (Log_ID  => "5B4C2FE0C5A7C700",
               Message => "Exception_Overflow: Traverse_Breadth_First failed");
            return;

         when OK =>
            null;
      end case;

      while not Vertex_Queue.Is_Empty (Vertices_Ready) loop
         declare
            Front_Of_Queue_Status : Vertex_Queue.Locus.Front_Of;
         begin
            Vertex_Queue.Front_Of
              (The_Queue    => Vertices_Ready,
               The_Front    => Temporary_Vertex,
               Booch_Status => Front_Of_Queue_Status);

            case Front_Of_Queue_Status is
               when Exception_Underflow =>
                  Booch_Status := Front_Of_Queue_Status;
                  Alogs.Log
                    (Log_ID  => "51CBFB3DDCBDD9F7",
                     Message =>
                       "Exception_Underflow: Traverse_Breadth_First failed");
                  return;

               when OK =>
                  null;
            end case;
         end;

         declare
            Pop_Status : Vertex_Queue.Locus.Pop;
         begin
            Vertex_Queue.Pop
              (The_Queue    => Vertices_Ready,
               Booch_Status => Pop_Status);

            case Pop_Status is
               when Exception_Underflow =>
                  Booch_Status := Pop_Status;
                  Alogs.Log
                    (Log_ID  => "C3BAD12B362D6745",
                     Message =>
                       "Exception_Underflow: Traverse_Breadth_First failed");
                  return;
               when OK =>
                  null;
            end case;
         end;

         Process (Temporary_Vertex, Continue);
         exit when not Continue;
         Initialize
           (The_Iterator,
            With_The_Vertex => Temporary_Vertex);
         while not Is_Done (The_Iterator) loop
            if not Vertex_Set.Is_A_Member
                (Value_Of (The_Iterator), Vertices_Visited)
            then

               Vertex_Set.Add
                 (The_Item     => Value_Of (The_Iterator),
                  To_The_Set   => Vertices_Visited,
                  Booch_Status => Add_Set_Status);

               case Add_Set_Status is
                  when Item_Is_In_Set | Exception_Overflow
                    | Exception_Storage_Error =>
                     Booch_Status := Add_Set_Status;
                     Alogs.Log
                       (Log_ID  => "5EE9D6ECDA2A974A",
                        Message => "Traverse_Breadth_First failed");
                     return;

                  when OK =>
                     null;
               end case;

               Vertex_Queue.Add
                 (The_Item     => Value_Of (The_Iterator),
                  To_The_Queue => Vertices_Ready,
                  Booch_Status => Add_Queue_Status);

               case Add_Queue_Status is
                  when Exception_Overflow =>
                     Booch_Status := Add_Queue_Status;
                     Alogs.Log
                       (Log_ID  => "55DE554E2C4AF12A",
                        Message =>
                          "Exception_Overflow: Traverse_Breadth_First failed");
                     return;

                  when OK =>
                     null;
               end case;

            end if;
            Get_Next (The_Iterator);
         end loop;
      end loop;
   end Traverse_Breadth_First;

end Booch_Light.Graph_Search_Bounded_Managed;

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
