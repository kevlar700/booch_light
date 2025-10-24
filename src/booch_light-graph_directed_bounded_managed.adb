--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Map_Simple_Noncached_Sequential_Bounded_Managed_Iterator;
with Booch_Light.Alogs;

package body Booch_Light.Graph_Directed_Bounded_Managed is

   type Vertex_Node is record
      The_Item        : Item;
      The_Arcs        : Arc_Set.Set (Number_Of_Arcs);
      Reference_Count : Natural := 0;
      Next            : Vertex;
   end record;

   type Arc_Node is record
      The_Attribute   : Attribute;
      The_Source      : Vertex;
      The_Destination : Vertex;
      Next            : Arc;
   end record;

   Vertex_Heap : array (Positive range 1 .. Number_Of_Vertices) of Vertex_Node;
   Arc_Heap    : array (Positive range 1 .. Number_Of_Arcs) of Arc_Node;

   Vertex_Free_List : Vertex;
   Arc_Free_List    : Arc;

   function Hash_Of
     (The_Vertex : Vertex)
      return Positive
   is
      pragma Unreferenced (The_Vertex);
   begin
      return 1;
   end Hash_Of;

   package Vertex_Map is new Map_Simple_Noncached_Sequential_Bounded_Managed_Iterator
     (Domain  => Vertex,
      Ranges  => Vertex,
      Hash_Of => Hash_Of);

   procedure Free (The_Vertex : in out Vertex) is
   begin
      if The_Vertex /= Null_Vertex
      then
         Arc_Set.Clear (Vertex_Heap (The_Vertex.The_Head).The_Arcs);
         Vertex_Heap (The_Vertex.The_Head).Reference_Count := 0;
         Vertex_Heap (The_Vertex.The_Head).Next            := Vertex_Free_List;
         Vertex_Free_List                                  := The_Vertex;
         The_Vertex                                        := Null_Vertex;
      end if;
   end Free;

   procedure New_Item
     (The_Item     : out Vertex;
      Booch_Status : out Locus.New_Item)
   is
      Temporary_Vertex : Vertex;
   begin
      if Vertex_Free_List = Null_Vertex
      then
         Alogs.Log
           (Log_ID  => "D0B31F93174E72A1",
            Message =>
              "No_Storage_Available: Vertex_Free_List indicates that all " &
              "space is used");
         Booch_Status := No_Storage_Available;
         return;
      else
         Temporary_Vertex                             := Vertex_Free_List;
         Vertex_Free_List := Vertex_Heap (Vertex_Free_List.The_Head).Next;
         Vertex_Heap (Temporary_Vertex.The_Head).Next := Null_Vertex;
         The_Item                                     := Temporary_Vertex;
      end if;

      Booch_Status := OK;

   end New_Item;

   procedure Free (The_Arc : in out Arc) is
   begin
      if The_Arc /= Null_Arc
      then
         Arc_Heap (The_Arc.The_Head).Next := Arc_Free_List;
         Arc_Free_List                    := The_Arc;
         The_Arc                          := Null_Arc;
      end if;
   end Free;

   procedure New_Item
     (The_Item     : out Arc;
      Booch_Status : out Locus.New_Item)
   is
      Temporary_Arc : Arc;
   begin
      if Arc_Free_List = Null_Arc
      then
         Alogs.Log
           (Log_ID  => "926ED38D301DBEC7",
            Message =>
              "No_Storage_Available: Arc_Free_List indicates that all space " &
              "is used");
         Booch_Status := No_Storage_Available;
         return;
      else
         Temporary_Arc                          := Arc_Free_List;
         Arc_Free_List := Arc_Heap (Arc_Free_List.The_Head).Next;
         Arc_Heap (Temporary_Arc.The_Head).Next := Null_Arc;
         The_Item                               := Temporary_Arc;
      end if;

      Booch_Status := OK;

   end New_Item;

   --  TODO: Replace as recursion is not permitted in this repo
   --  procedure Copy
   --    (From_The_Graph :        Graph;
   --     To_The_Graph   : in out Graph;
   --     Booch_Status   :    out Locus.Copy)
   --
   --  is
   --     New_Item_Status  : Locus.New_Item;
   --     Vertices_Visited : Vertex_Map.Map (From_The_Graph.Total_Vertices);
   --     procedure Visit
   --       (The_Vertex   :     Vertex;
   --        Continue     : out Boolean;
   --        Booch_Status : out Locus.Copy)
   --     is
   --        Bind_Status      : Vertex_Map.Locus.Bind;
   --        Add_Status       : Vertex_Set.Locus.Add;
   --        Temporary_Vertex : Vertex;
   --
   --        procedure Duplicate
   --          (The_Arc      :     Arc;
   --           Continue     : out Boolean;
   --           Booch_Status : out Locus.Copy)
   --        is
   --           Temporary_Arc   : Arc;
   --           Set_Status      : Arc_Set.Locus.Add;
   --           New_Item_Status : Locus.New_Item;
   --        begin -- Duplicate
   --
   --           New_Item
   --             (The_Item     => Temporary_Arc,
   --              Booch_Status => New_Item_Status);
   --           case New_Item_Status is
   --              when No_Storage_Available =>
   --                 Alogs.Log
   --                   (Log_ID  => "83280D75475FB3D6",
   --                    Message => "Failed to create Temporary_Arc");
   --                 Booch_Status := New_Item_Status;
   --                 Continue     := False;
   --                 return;
   --
   --              when OK =>
   --                 null;
   --           end case;
   --
   --           Arc_Heap (Temporary_Arc.The_Head).The_Attribute :=
   --             Arc_Heap (The_Arc.The_Head).The_Attribute;
   --
   --           Arc_Heap (Temporary_Arc.The_Head).The_Source := Temporary_Vertex;
   --
   --           declare
   --              Visit_Status : Locus.Visit;
   --           begin
   --              Visit
   --                (The_Vertex   => Arc_Heap (The_Arc.The_Head).The_Destination,
   --                 Continue     => Continue,
   --                 Booch_Status => Visit_Status);
   --              case Visit_Status is
   --                 when Domain_Is_Not_Bound | Item_Is_In_Set
   --                   | Exception_Overflow | Exception_Storage_Error
   --                   | Multiple_Binding =>
   --                    Alogs.Log
   --                      (Log_ID  => "D5D4FFA4400C4637",
   --                       Message => "Failed to Visit The_Destination");
   --                    Continue     := False;
   --                    Booch_Status := Visit_Status;
   --                    return;
   --
   --                 when OK =>
   --                    null;
   --              end case;
   --
   --           end;
   --
   --           declare
   --              Map_Status        : Vertex_Map.Locus.Range_Of;
   --              Vertex_Map_Ranges : Vertex;
   --           begin
   --
   --              Vertex_Map.Range_Of
   --                (The_Domain   => Arc_Heap (The_Arc.The_Head).The_Destination,
   --                 In_The_Map   => Vertices_Visited,
   --                 Result       => Vertex_Map_Ranges,
   --                 Booch_Status => Map_Status);
   --
   --              case Map_Status is
   --                 when Domain_Is_Not_Bound =>
   --                    Alogs.Log
   --                      (Log_ID  => "168273D4496FD173",
   --                       Message =>
   --                         "Failed to Vertex_Map.Range_Of The_Destination");
   --                    Booch_Status := Map_Status;
   --                    Continue     := False;
   --                    return;
   --
   --                 when OK =>
   --                    Arc_Heap (Temporary_Arc.The_Head).The_Destination :=
   --                      Vertex_Map_Ranges;
   --              end case;
   --           end;
   --
   --           Arc_Set.Add
   --             (The_Item     => Temporary_Arc,
   --              To_The_Set => Vertex_Heap (Temporary_Vertex.The_Head).The_Arcs,
   --              Booch_Status => Set_Status);
   --
   --           case Set_Status is
   --              when Item_Is_In_Set | Exception_Overflow =>
   --                 Alogs.Log
   --                   (Log_ID  => "113055E039189CC5",
   --                    Message => "Failed to add Arc to Vertex_Heap");
   --                 Booch_Status := Set_Status;
   --                 Continue     := False;
   --                 return;
   --
   --              when OK =>
   --                 null;
   --           end case;
   --
   --           Arc_Set.Add
   --             (The_Item     => Temporary_Arc,
   --              To_The_Set   => To_The_Graph.The_Arcs,
   --              Booch_Status => Set_Status);
   --           case Set_Status is
   --              when Item_Is_In_Set | Exception_Overflow =>
   --                 Alogs.Log
   --                   (Log_ID  => "63F395FA5664C067",
   --                    Message => "Failed to add Arc To_The_Graph");
   --                 Booch_Status := Set_Status;
   --                 Continue     := False;
   --                 return;
   --
   --              when OK =>
   --                 null;
   --           end case;
   --
   --           Booch_Status := OK;
   --
   --        end Duplicate;
   --
   --        procedure Process_Duplicate is new Arc_Set.Iterate_With_Status
   --          (Process     => Duplicate,
   --           Status_Item => Locus.Copy);
   --
   --     begin --  Visit
   --        if not Vertex_Map.Is_Bound
   --            (The_Vertex,
   --             In_The_Map => Vertices_Visited)
   --        then
   --
   --           New_Item
   --             (The_Item     => Temporary_Vertex,
   --              Booch_Status => New_Item_Status);
   --
   --           case New_Item_Status is
   --              when No_Storage_Available =>
   --                 Alogs.Log
   --                   (Log_ID  => "F47E1C978103042E",
   --                    Message => "Not enough storage to add Vertex");
   --                 Booch_Status := New_Item_Status;
   --                 Continue     := False;
   --                 return;
   --
   --              when OK =>
   --                 null;
   --           end case;
   --
   --           Vertex_Heap (Temporary_Vertex.The_Head).The_Item :=
   --             Vertex_Heap (The_Vertex.The_Head).The_Item;
   --
   --           Vertex_Heap (Temporary_Vertex.The_Head).Reference_Count :=
   --             Vertex_Heap (The_Vertex.The_Head).Reference_Count;
   --
   --           Vertex_Map.Bind
   --             (The_Domain    => The_Vertex,
   --              And_The_Range => Temporary_Vertex,
   --              In_The_Map    => Vertices_Visited,
   --              Booch_Status  => Bind_Status);
   --
   --           case Bind_Status is
   --              when Exception_Overflow | Multiple_Binding =>
   --                 Alogs.Log
   --                   (Log_ID  => "D4529FA9EEFD388E",
   --                    Message => "Failed to Bind The_Vertex");
   --                 Booch_Status := Bind_Status;
   --                 Continue     := False;
   --                 return;
   --
   --              when OK =>
   --                 null;
   --           end case;
   --
   --           Vertex_Set.Add
   --             (The_Item     => Temporary_Vertex,
   --              To_The_Set   => To_The_Graph.The_Vertices,
   --              Booch_Status => Add_Status);
   --
   --           case Add_Status is
   --              when Item_Is_In_Set | Exception_Overflow =>
   --                 Alogs.Log
   --                   (Log_ID  => "B3AA5CA049C5F8A1",
   --                    Message => "Failed to Add Vertex To_The_Graph");
   --                 Booch_Status := Add_Status;
   --                 Continue     := False;
   --                 return;
   --
   --              when OK =>
   --                 null;
   --           end case;
   --
   --           declare
   --              Status_Duplicate : Locus.Duplicate;
   --           begin
   --              Process_Duplicate
   --                (Over_The_Set => Vertex_Heap (The_Vertex.The_Head).The_Arcs,
   --                 Booch_Status => Status_Duplicate);
   --
   --              case Status_Duplicate is
   --                 when Domain_Is_Not_Bound | Item_Is_In_Set
   --                   | Exception_Overflow | Exception_Storage_Error =>
   --                    Alogs.Log
   --                      (Log_ID  => "4C02FB94920AD302",
   --                       Message =>
   --                         "Error whilst processing Duplicate over The_ARC");
   --                    Continue := False;
   --                    return;
   --
   --                 when OK =>
   --                    null;
   --              end case;
   --
   --           end;
   --        end if;
   --        Continue     := True;
   --        Booch_Status := OK;
   --     end Visit;
   --
   --     procedure Traverse is new Vertex_Set.Iterate_With_Status
   --       (Process     => Visit,
   --        Status_Item => Locus.Copy);
   --
   --  begin --  Copy
   --     Clear (To_The_Graph);
   --     Traverse (From_The_Graph.The_Vertices, Booch_Status);
   --
   --  exception
   --
   --     when Storage_Error =>
   --        Alogs.Status_Exception
   --          (Log_ID  => "0E5B139E3428329D",
   --           Message => "Error whilst processing Duplicate over The_ARC");
   --        Booch_Status := Exception_Storage_Error;
   --        return;
   --
   --  end Copy;

   procedure Clear (The_Graph : in out Graph) is
      procedure Remove_Vertex
        (The_Vertex :     Vertex;
         Continue   : out Boolean)
      is
         Temporary_Vertex : Vertex := The_Vertex;
      begin
         Free (Temporary_Vertex);
         Continue := True;
      end Remove_Vertex;

      procedure Traverse is new Vertex_Set.Iterate (Remove_Vertex);

      procedure Remove_Arc
        (The_Arc  :     Arc;
         Continue : out Boolean)
      is
         Temporary_Arc : Arc := The_Arc;
      begin
         Free (Temporary_Arc);
         Continue := True;
      end Remove_Arc;
      procedure Traverse is new Arc_Set.Iterate (Remove_Arc);
   begin
      Traverse (The_Graph.The_Vertices);
      Traverse (The_Graph.The_Arcs);
      Vertex_Set.Clear (The_Graph.The_Vertices);
      Arc_Set.Clear (The_Graph.The_Arcs);
   end Clear;

   procedure Add
     (The_Vertex    : in out Vertex;
      With_The_Item :        Item;
      To_The_Graph  : in out Graph;
      Booch_Status  :    out Locus.Add)
   is
      Add_Status      : Vertex_Set.Locus.Add;
      New_Item_Status : Locus.New_Item;
   begin
      New_Item
        (The_Item     => The_Vertex,
         Booch_Status => New_Item_Status);

      case New_Item_Status is
         when No_Storage_Available =>
            Alogs.Log
              (Log_ID  => "E624E1876CCC1A29",
               Message => "No_Storage_Available: to Add The_Vertex");
            Booch_Status := New_Item_Status;
            return;

         when OK =>
            null;
      end case;

      Vertex_Heap (The_Vertex.The_Head).The_Item := With_The_Item;
      Vertex_Set.Add
        (The_Item     => The_Vertex,
         To_The_Set   => To_The_Graph.The_Vertices,
         Booch_Status => Add_Status);

      case Add_Status is
         when Item_Is_In_Set | Exception_Overflow =>
            Alogs.Log
              (Log_ID  => "C8914B1BE42BDA0A",
               Message => "Unable to Add The_Vertex");
            Booch_Status := Add_Status;
            return;

         when OK =>
            null;
      end case;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alogs.Status_Exception
           (Log_ID  => "966190A2E2444B6C",
            Message => "Storage_Error: Failed to Add The_Vertex");
         Booch_Status := Exception_Storage_Error;
         return;

   end Add;

   procedure Remove
     (The_Vertex     : in out Vertex;
      From_The_Graph : in out Graph;
      Booch_Status   :    out Locus.Remove)
   is
      Remove_Status : Vertex_Set.Locus.Remove;
      procedure Remove_Arc
        (The_Arc      :     Arc;
         Continue     : out Boolean;
         Booch_Status : out Locus.Remove)
      is
         Remove_Status : Arc_Set.Locus.Remove;
         Temporary_Arc : Arc := The_Arc;
      begin
         Vertex_Heap (Arc_Heap (The_Arc.The_Head).The_Destination.The_Head)
           .Reference_Count :=
           Vertex_Heap (Arc_Heap (The_Arc.The_Head).The_Destination.The_Head)
             .Reference_Count -
           1;

         Arc_Set.Remove
           (The_Item     => The_Arc,
            From_The_Set => From_The_Graph.The_Arcs,
            Booch_Status => Remove_Status);

         case Remove_Status is
            when Item_Is_Not_In_Set =>
               Alogs.Log
                 (Log_ID  => "3E33758AC76CAD8A",
                  Message => "Failed to remove The_Arc");
               Continue     := False;
               Booch_Status := Remove_Status;
               return;

            when OK =>
               null;
         end case;

         Free (Temporary_Arc);
         Booch_Status := OK;
         Continue     := True;
      end Remove_Arc;

      procedure Traverse is new Arc_Set.Iterate_With_Status
        (Process     => Remove_Arc,
         Status_Item => Locus.Copy);

   begin
      if Vertex_Heap (The_Vertex.The_Head).Reference_Count /= 0
      then
         Booch_Status := Vertex_Has_References;
         Alogs.Log
           (Log_ID  => "2E7CE31F3C9EC16F",
            Message => "Vertex_Has_References: Remove failed");
         return;
      elsif not Vertex_Set.Is_A_Member
          (The_Vertex, From_The_Graph.The_Vertices)
      then
         Booch_Status := Vertex_Is_Not_In_Graph;
         Alogs.Log
           (Log_ID  => "30923A85E1882575",
            Message => "Vertex_Is_Not_In_Graph: Remove failed");
         return;
      else
         Traverse (Vertex_Heap (The_Vertex.The_Head).The_Arcs, Booch_Status);
         if Booch_Status not in OK
         then
            return;
         end if;

         Vertex_Set.Remove
           (The_Item     => The_Vertex,
            From_The_Set => From_The_Graph.The_Vertices,
            Booch_Status => Remove_Status);

         case Remove_Status is
            when Item_Is_Not_In_Set =>
               Alogs.Log
                 (Log_ID  => "13BE9B90595FD6E5",
                  Message => "Failed to Remove The_Vertex");
               Booch_Status := Remove_Status;
               return;

            when OK =>
               null;
         end case;

         Free (The_Vertex);
      end if;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "A77E20F670A35991",
            Message => "Constraint_Error: Failed to Remove The_Vertex");
         Booch_Status := Vertex_Is_Null;
         return;
   end Remove;

   procedure Set_Item
     (Of_The_Vertex : in out Vertex;
      To_The_Item   :        Item;
      Booch_Status  :    out Locus.Set_Item)
   is
   begin
      Vertex_Heap (Of_The_Vertex.The_Head).The_Item := To_The_Item;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Booch_Status := Vertex_Is_Null;
         Alogs.Log
           (Log_ID  => "E06B9318BDA2DEC4",
            Message => "Vertex_Is_Null: Set_Item failed");
         return;

   end Set_Item;

   procedure Create
     (The_Arc            : in out Arc;
      With_The_Attribute :        Attribute;
      From_The_Vertex    : in out Vertex;
      To_The_Vertex      :        Vertex;
      In_The_Graph       : in out Graph;
      Booch_Status       :    out Locus.Create)
   is
      Add_Status      : Arc_Set.Locus.Add;
      New_Item_Status : Locus.New_Item;
   begin
      if (From_The_Vertex = Null_Vertex) or else (To_The_Vertex = Null_Vertex)
      then
         Booch_Status := Vertex_Is_Null;
         Alogs.Log
           (Log_ID  => "F6D09C7CB7B57B61",
            Message => "Vertex_Is_Null: Create failed");
         return;
      elsif not
        (Vertex_Set.Is_A_Member (From_The_Vertex, In_The_Graph.The_Vertices)
         and then Vertex_Set.Is_A_Member
           (To_The_Vertex, In_The_Graph.The_Vertices))
      then
         Booch_Status := Vertex_Is_Not_In_Graph;
         Alogs.Log
           (Log_ID  => "1A396D7A6D48188E",
            Message => "Vertex_Is_Not_In_Graph: Create failed");
         return;
      else

         New_Item
           (The_Item     => The_Arc,
            Booch_Status => New_Item_Status);

         case New_Item_Status is
            when No_Storage_Available =>
               Alogs.Log
                 (Log_ID  => "C71396981E922060",
                  Message => "Failed to Create The_Arc in The_Graph");
               Booch_Status := New_Item_Status;
               return;
            when OK =>
               null;
         end case;

         Arc_Heap (The_Arc.The_Head).The_Attribute   := With_The_Attribute;
         Arc_Heap (The_Arc.The_Head).The_Source      := From_The_Vertex;
         Arc_Heap (The_Arc.The_Head).The_Destination := To_The_Vertex;

         Arc_Set.Add
           (The_Item     => The_Arc,
            To_The_Set   => In_The_Graph.The_Arcs,
            Booch_Status => Add_Status);

         case Add_Status is
            when Item_Is_In_Set | Exception_Overflow =>
               Alogs.Log
                 (Log_ID  => "FB02B331B381F524",
                  Message => "Failed to Create The_Arc in The_Graph");
               Booch_Status := Add_Status;
               return;

            when OK =>
               null;
         end case;

         Arc_Set.Add
           (The_Item     => The_Arc,
            To_The_Set   => Vertex_Heap (From_The_Vertex.The_Head).The_Arcs,
            Booch_Status => Add_Status);

         case Add_Status is
            when Item_Is_In_Set | Exception_Overflow =>
               Alogs.Log
                 (Log_ID  => "8C501B93233413D0",
                  Message => "Failed to Create The_Arc in The_Graph");
               Booch_Status := Add_Status;
               return;

            when OK =>
               null;
         end case;

         if From_The_Vertex /= To_The_Vertex
         then
            Vertex_Heap (To_The_Vertex.The_Head).Reference_Count :=
              Vertex_Heap (To_The_Vertex.The_Head).Reference_Count + 1;
         end if;
      end if;

      Booch_Status := OK;

   exception
      when Storage_Error =>
         Alogs.Status_Exception
           (Log_ID  => "1B45FB165F040EEB",
            Message => "Constraint_Error: Failed to Create The_Arc");
         Booch_Status := Exception_Storage_Error;
         return;
   end Create;

   procedure Destroy
     (The_Arc      : in out Arc;
      In_The_Graph : in out Graph;
      Booch_Status :    out Locus.Destroy)
   is
      Remove_Status : Arc_Set.Locus.Remove;
   begin
      if The_Arc = Null_Arc
      then
         Booch_Status := Arc_Is_Null;
         Alogs.Log
           (Log_ID => "B16E6EDA52A488B4",

            Message => "Arc_Is_Null: Destroy failed");
         return;
      elsif not Arc_Set.Is_A_Member (The_Arc, In_The_Graph.The_Arcs)
      then
         Booch_Status := Arc_Is_Not_In_Graph;
         Alogs.Log
           (Log_ID  => "B59D2D9BA0834DEC",
            Message => "Arc_Is_Not_In_Graph: Destroy failed");
         return;
      else
         Arc_Set.Remove
           (The_Item     => The_Arc,
            From_The_Set =>
              Vertex_Heap (Arc_Heap (The_Arc.The_Head).The_Source.The_Head)
                .The_Arcs,
            Booch_Status => Remove_Status);

         case Remove_Status is
            when Item_Is_Not_In_Set =>
               Booch_Status := Remove_Status;
               Alogs.Log
                 (Log_ID  => "86AD031E3D413599",
                  Message => "Item_Is_Not_In_Set: Destroy failed");
               return;

            when OK =>
               null;
         end case;

         if Arc_Heap (The_Arc.The_Head).The_Source /=
           Arc_Heap (The_Arc.The_Head).The_Destination
         then
            Vertex_Heap (Arc_Heap (The_Arc.The_Head).The_Destination.The_Head)
              .Reference_Count :=
              Vertex_Heap
                (Arc_Heap (The_Arc.The_Head).The_Destination.The_Head)
                .Reference_Count -
              1;
         end if;

         Arc_Set.Remove
           (The_Item     => The_Arc,
            From_The_Set => In_The_Graph.The_Arcs,
            Booch_Status => Remove_Status);
         case Remove_Status is
            when Item_Is_Not_In_Set =>
               Alogs.Log
                 (Log_ID  => "53ECC7DA9C4BECDB",
                  Message => "Failed to Remove The_Arc from The_Graph");
               Booch_Status := Remove_Status;
               return;
            when OK =>
               null;
         end case;

         Free (The_Arc);
      end if;

      Booch_Status := OK;

   end Destroy;

   procedure Set_Attribute
     (Of_The_Arc       : in out Arc;
      To_The_Attribute :        Attribute;
      Booch_Status     :    out Locus.Set_Attribute)
   is
   begin
      Arc_Heap (Of_The_Arc.The_Head).The_Attribute := To_The_Attribute;

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Log
           (Log_ID  => "E28BB51C0E077FE1",
            Message => "Arc_Is_Null: Set_Attribute failed");
         Booch_Status := Arc_Is_Null;
         return;

   end Set_Attribute;

   procedure Number_Of_Arcs_From
     (The_Vertex    :     Vertex;
      The_Arc_Count : out Natural;
      Booch_Status  : out Locus.Number_Of_Arcs_From)
   is
   begin
      The_Arc_Count :=
        Arc_Set.Extent_Of (Vertex_Heap (The_Vertex.The_Head).The_Arcs);

      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "1F17E58486153BA7",
            Message =>
              "Constraint_Error: Number_Of_Arcs_From failed due to" &
              "null vertex");
         Booch_Status := Vertex_Is_Null;
         return;

   end Number_Of_Arcs_From;

   function Is_Empty
     (The_Graph : Graph)
      return Boolean
   is
   begin
      return Vertex_Set.Is_Empty (The_Graph.The_Vertices);
   end Is_Empty;

   function Is_Null
     (The_Vertex : Vertex)
      return Boolean
   is
   begin
      return (The_Vertex = Null_Vertex);
   end Is_Null;

   function Is_Null
     (The_Arc : Arc)
      return Boolean
   is
   begin
      return (The_Arc = Null_Arc);
   end Is_Null;

   function Number_Of_Vertices_In
     (The_Graph : Graph)
      return Natural
   is
   begin
      return Vertex_Set.Extent_Of (The_Graph.The_Vertices);
   end Number_Of_Vertices_In;

   function Number_Of_Arcs_In
     (The_Graph : Graph)
      return Natural
   is
   begin
      return Arc_Set.Extent_Of (The_Graph.The_Arcs);
   end Number_Of_Arcs_In;

   procedure Item_Of
     (The_Vertex   :     Vertex;
      The_Item     : out Item;
      Booch_Status : out Locus.Item_Of)
   is
   begin
      The_Item     := Vertex_Heap (The_Vertex.The_Head).The_Item;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "28F166ADC9C9C05B",
            Message => "Constraint_Error: Item_Of failed due to null vertex");
         Booch_Status := Vertex_Is_Null;
         return;

   end Item_Of;

   procedure Attribute_Of
     (The_Arc       :     Arc;
      The_Attribute : out Attribute;
      Booch_Status  : out Locus.Attribute_Of)
   is
   begin
      The_Attribute := Arc_Heap (The_Arc.The_Head).The_Attribute;
      Booch_Status  := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "558A6844277E4346",
            Message => "Constraint_Error: Item_Of failed due to null arc");
         Booch_Status := Arc_Is_Null;

   end Attribute_Of;

   procedure Source_Of
     (The_Arc      :     Arc;
      The_Source   : out Vertex;
      Booch_Status : out Locus.Source_Of)
   is
   begin
      The_Source   := Arc_Heap (The_Arc.The_Head).The_Source;
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "558A6844277E4346",
            Message => "Constraint_Error: Source_Of failed due to null arc");
         Booch_Status := Arc_Is_Null;
         return;

   end Source_Of;

   procedure Destination_Of
     (The_Arc         :     Arc;
      The_Destination : out Vertex;
      Booch_Status    : out Locus.Destination_Of)
   is
   begin
      The_Destination := Arc_Heap (The_Arc.The_Head).The_Destination;
      Booch_Status    := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "8C00B2193DCF94F8",
            Message =>
              "Constraint_Error: Destination_Of failed due to null arc");
         Booch_Status := Arc_Is_Null;
         return;

   end Destination_Of;

   function Is_A_Member
     (The_Vertex   : Vertex;
      Of_The_Graph : Graph)
      return Boolean
   is
   begin
      return Vertex_Set.Is_A_Member (The_Vertex, Of_The_Graph.The_Vertices);
   end Is_A_Member;

   function Is_A_Member
     (The_Arc      : Arc;
      Of_The_Graph : Graph)
      return Boolean
   is
   begin
      return Arc_Set.Is_A_Member (The_Arc, Of_The_Graph.The_Arcs);
   end Is_A_Member;

   procedure Iterate_Vertices (Over_The_Graph : Graph) is
      procedure Traverse is new Vertex_Set.Iterate (Process);
   begin
      Traverse (Over_The_Graph.The_Vertices);
   end Iterate_Vertices;

   procedure Iterate_Arcs (Over_The_Graph : Graph) is
      procedure Traverse is new Arc_Set.Iterate (Process);
   begin
      Traverse (Over_The_Graph.The_Arcs);
   end Iterate_Arcs;

   procedure Reiterate
     (Over_The_Vertex :     Vertex;
      Booch_Status    : out Locus.Reiterate)
   is
      procedure Traverse is new Arc_Set.Iterate (Process);
   begin
      Traverse (Vertex_Heap (Over_The_Vertex.The_Head).The_Arcs);
      Booch_Status := OK;

   exception
      when Constraint_Error =>
         Alogs.Status_Exception
           (Log_ID  => "0EECBFDA50F02DA1",
            Message =>
              "Constraint_Error: Reiterate failed due to null vertex");
         Booch_Status := Vertex_Is_Null;
         return;

   end Reiterate;

begin
   Vertex_Free_List.The_Head := 1;
   for Index in 1 .. Number_Of_Vertices - 1 loop
      Vertex_Heap (Index).Next := Vertex'(The_Head => (Index + 1));
   end loop;
   Vertex_Heap (Number_Of_Vertices).Next := Null_Vertex;
   Arc_Free_List.The_Head                := 1;
   for Index in 1 .. Number_Of_Arcs - 1 loop
      Arc_Heap (Index).Next := Arc'(The_Head => (Index + 1));
   end loop;
   Arc_Heap (Number_Of_Arcs).Next := Null_Arc;
end Booch_Light.Graph_Directed_Bounded_Managed;

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
