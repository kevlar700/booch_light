--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

with Booch_Light.Alogs;

package body Booch_Light.Pattern_Match_Regular_Expression is

   procedure Location_Of
     (The_Pattern  : in     Items;
      In_The_Items : in     Items;
      Result       :    out Index;
      Booch_Status :    out Locus.Location_Of)
   is

      type Kind is
        (Literal,
         Class,
         Any,
         Stop,
         Unknown);
      type Literals is array (Positive range 1 .. The_Pattern'Length) of Item;
      type Pattern (The_Kind : Kind := Unknown) is record
         True_Pattern : Boolean := True;
         Has_Closure  : Boolean := False;
         case The_Kind is
            when Literal =>
               The_Item : Item;
            when Class =>
               Number_Of_Items : Natural := 0;
               The_Items       : Literals;
            when Any | Stop | Unknown =>
               null;
         end case;
      end record;
      type Patterns is array (Positive range <>) of Pattern;

      Full_Pattern : Patterns (1 .. The_Pattern'Length + 1);

      procedure Preprocess
        (The_Pattern  : in     Items;
         Full_Pattern : in out Patterns;
         Booch_Status :    out Locus.Preprocess)
      is
         type State is
           (Building_Pattern,
            Building_Class,
            Building_Escape_Pattern,
            Building_Escape_Class);
         The_State     : State    := Building_Pattern;
         Pattern_Index : Index    := The_Pattern'First;
         Full_Index    : Positive := Full_Pattern'First;
         Last_Pattern  : Natural  := 0;
      begin
         loop
            case The_State is
               when Building_Pattern =>
                  if Is_Equal (The_Pattern (Pattern_Index), Any_Item) then
                     if Full_Pattern (Full_Index).True_Pattern then
                        Full_Pattern (Full_Index) :=
                          (The_Kind     => Any,
                           True_Pattern =>
                             Full_Pattern (Full_Index).True_Pattern,
                           Has_Closure  => False);
                        Last_Pattern              := Full_Index;
                        Full_Index                := Full_Index + 1;
                     else
                        Alogs.Log
                          (Log_ID  => "5E95CE44668D6FFC",
                           Message => "Illegal_Pattern: Preprocess failed");
                        Booch_Status := Illegal_Pattern;
                        return;
                     end if;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Escape_Item)
                  then
                     The_State := Building_Escape_Pattern;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Not_Item) then
                     if Full_Pattern (Full_Index).True_Pattern then
                        Full_Pattern (Full_Index).True_Pattern := False;
                     else
                        Alogs.Log
                          (Log_ID  => "03C1D69BCE6A0EFD",
                           Message => "Illegal_Pattern: Preprocess failed");
                        Booch_Status := Illegal_Pattern;
                        return;
                     end if;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Closure_Item)
                  then
                     if not Full_Pattern (Last_Pattern).Has_Closure then
                        Full_Pattern (Last_Pattern).Has_Closure := True;
                     else
                        Alogs.Log
                          (Log_ID  => "9A9F379927C3BF9A",
                           Message => "Illegal_Pattern: Preprocess failed");
                        Booch_Status := Illegal_Pattern;
                        return;
                     end if;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Start_Class)
                  then
                     Full_Pattern (Full_Index) :=
                       (The_Kind        => Class,
                        True_Pattern => Full_Pattern (Full_Index).True_Pattern,
                        Has_Closure     => False,
                        Number_Of_Items => 0,
                        The_Items       =>
                          (others => Any_Item));
                     Last_Pattern              := Full_Index;
                     Full_Index                := Full_Index + 1;
                     The_State                 := Building_Class;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Stop_Class) then
                     Alogs.Log
                       (Log_ID  => "033AFC90B03DA0C2",
                        Message => "Illegal_Pattern: Preprocess failed");
                     Booch_Status := Illegal_Pattern;
                     return;
                  else
                     Full_Pattern (Full_Index) :=
                       (The_Kind     => Literal,
                        True_Pattern => Full_Pattern (Full_Index).True_Pattern,
                        Has_Closure  => False,
                        The_Item     => The_Pattern (Pattern_Index));
                     Last_Pattern              := Full_Index;
                     Full_Index                := Full_Index + 1;
                  end if;
               when Building_Class =>
                  if Is_Equal (The_Pattern (Pattern_Index), Any_Item) then
                     Alogs.Log
                       (Log_ID  => "BED689DCA789D0F2",
                        Message => "Illegal_Pattern: Preprocess failed");
                     Booch_Status := Illegal_Pattern;
                     return;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Escape_Item)
                  then
                     The_State := Building_Escape_Class;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Not_Item) then
                     Alogs.Log
                       (Log_ID  => "7FE0D72EF3599611",
                        Message => "Illegal_Pattern: Preprocess failed");
                     Booch_Status := Illegal_Pattern;
                     return;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Closure_Item)
                  then
                     Alogs.Log
                       (Log_ID  => "DD94EAF685B514AA",
                        Message => "Illegal_Pattern: Preprocess failed");
                     Booch_Status := Illegal_Pattern;
                     return;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Start_Class)
                  then
                     Alogs.Log
                       (Log_ID  => "47A71011CE3E0011",
                        Message => "Illegal_Pattern: Preprocess failed");
                     Booch_Status := Illegal_Pattern;
                     return;
                  elsif Is_Equal (The_Pattern (Pattern_Index), Stop_Class) then
                     if Full_Pattern (Last_Pattern).Number_Of_Items > 0 then
                        The_State := Building_Pattern;
                     else
                        Alogs.Log
                          (Log_ID  => "A4A1B8F876CAFA61",
                           Message => "Illegal_Pattern: Preprocess failed");
                        Booch_Status := Illegal_Pattern;
                        return;
                     end if;
                  else
                     Full_Pattern (Last_Pattern).Number_Of_Items     :=
                       Full_Pattern (Last_Pattern).Number_Of_Items + 1;
                     Full_Pattern (Last_Pattern).The_Items
                       (Full_Pattern (Last_Pattern).Number_Of_Items) :=
                       The_Pattern (Pattern_Index);
                  end if;
               when Building_Escape_Pattern =>
                  Full_Pattern (Full_Index) :=
                    (The_Kind     => Literal,
                     True_Pattern => Full_Pattern (Full_Index).True_Pattern,
                     Has_Closure  => False,
                     The_Item     => The_Pattern (Pattern_Index));
                  Last_Pattern              := Full_Index;
                  Full_Index                := Full_Index + 1;
                  The_State                 := Building_Pattern;
               when Building_Escape_Class =>
                  Full_Pattern (Last_Pattern).Number_Of_Items     :=
                    Full_Pattern (Last_Pattern).Number_Of_Items + 1;
                  Full_Pattern (Last_Pattern).The_Items
                    (Full_Pattern (Last_Pattern).Number_Of_Items) :=
                    The_Pattern (Pattern_Index);
                  The_State := Building_Class;
            end case;
            if Pattern_Index = The_Pattern'Last then
               if (The_State = Building_Pattern)
                 and then (Full_Pattern (Full_Index).True_Pattern)
               then
                  Full_Pattern (Full_Index) :=
                    (The_Kind     => Stop,
                     True_Pattern => Full_Pattern (Full_Index).True_Pattern,
                     Has_Closure  => False);
                  Booch_Status              := OK;
                  return;
               else
                  Alogs.Log
                    (Log_ID  => "04453A84889FBB9F",
                     Message => "Illegal_Pattern: Preprocess failed");
                  Booch_Status := Illegal_Pattern;
                  return;
               end if;
            else
               Pattern_Index := Index'Succ (Pattern_Index);
            end if;
         end loop;
      exception
         when Constraint_Error =>
            Alogs.Status_Exception
              (Log_ID  => "5BAD581EDCE92B23",
               Message =>
                 "Constraint_Error: Illegal_Pattern: Preprocess failed");
            Booch_Status := Illegal_Pattern;
            return;
      end Preprocess;

      procedure Is_Match
        (The_Pattern   : in     Pattern;
         The_Item      : in     Item;
         Result        :    out Boolean;
         Nested_Status :    out Locus.Is_Match)
      is
      begin
         case The_Pattern.The_Kind is
            when Literal =>
               if The_Pattern.True_Pattern then
                  Result        := Is_Equal (The_Pattern.The_Item, The_Item);
                  Nested_Status := OK;
                  return;
               else
                  Result := not Is_Equal (The_Pattern.The_Item, The_Item);
                  Nested_Status := OK;
                  return;
               end if;
            when Class =>
               if The_Pattern.True_Pattern then
                  for Index in 1 .. The_Pattern.Number_Of_Items loop
                     if Is_Equal (The_Pattern.The_Items (Index), The_Item) then
                        Result        := True;
                        Nested_Status := OK;
                        return;
                     end if;
                  end loop;
                  Result        := False;
                  Nested_Status := OK;
                  return;
               else
                  for Index in 1 .. The_Pattern.Number_Of_Items loop
                     if Is_Equal (The_Pattern.The_Items (Index), The_Item) then
                        Result        := False;
                        Nested_Status := OK;
                        return;
                     end if;
                  end loop;
                  Result        := True;
                  Nested_Status := OK;
                  return;
               end if;
            when Any =>
               Result        := True;
               Nested_Status := OK;
               return;

            when others =>
               Alogs.Status_Exception
                 (Log_ID  => "24496C6D790919C4",
                  Message => "Illegal_Pattern: Is_Match failed");
               Nested_Status := Illegal_Pattern;
               Result        := Boolean'Last;
               return;
         end case;
      end Is_Match;

      procedure Location_Of
        (Full_Pattern : in     Patterns;
         In_The_Items : in     Items;
         The_Start    : in     Index;
         Result       :    out Index;
         Booch_Status :    out Locus.Location_Of)
      is
         Items_Index        : Index   := The_Start;
         Total_Closures     : Natural := 0;
         Temporary_Location : Index;
         Temporary_Index    : Index;
         Pattern_Matched    : Boolean;
         Is_Match_Status    : Locus.Is_Match;
      begin
         for Full_Index in Full_Pattern'Range loop
            if Full_Pattern (Full_Index).The_Kind = Stop then
               Result       := The_Start;
               Booch_Status := OK;
               return;
            elsif Full_Pattern (Full_Index).Has_Closure then
               for Index in Items_Index .. In_The_Items'Last loop
                  Is_Match
                    (The_Pattern   => Full_Pattern (Full_Index),
                     The_Item      => In_The_Items (Index),
                     Result        => Pattern_Matched,
                     Nested_Status => Is_Match_Status);

                  case Is_Match_Status is
                     when Illegal_Pattern =>
                        Alogs.Log
                          (Log_ID  => "2E8FD2C437406BA4",
                           Message => "Illegal_Pattern: Location_Of failed");
                        Booch_Status := Is_Match_Status;
                        return;

                     when OK =>
                        null;

                  end case;

                  if Pattern_Matched then
                     Total_Closures := Total_Closures + 1;
                  else
                     exit;
                  end if;
               end loop;

               while Total_Closures > 0 loop
                  Temporary_Index :=
                    Index'Val (Index'Pos (Items_Index) + Total_Closures);

                  Location_Of
                    (Full_Pattern =>
                       Full_Pattern (Full_Index + 1 .. Full_Pattern'Last),
                     In_The_Items =>
                       In_The_Items (Temporary_Index .. In_The_Items'Last),
                     The_Start    => Temporary_Index,
                     Result       => Temporary_Location,
                     Booch_Status => Booch_Status);

                  case Booch_Status is
                     when Illegal_Pattern =>
                        Alogs.Log
                          (Log_ID  => "FD7F690F56828247",
                           Message =>
                             "Illegal_Pattern: Recursive Location_Of failed");
                        Booch_Status := Exception_Underflow;
                        return;

                     when Pattern_Not_Found =>
                        Total_Closures := Total_Closures - 1;

                     when OK =>
                        Items_Index := Temporary_Index;
                        exit;
                  end case;

               end loop;
            end if;

            Is_Match
              (The_Pattern   => Full_Pattern (Full_Index),
               The_Item      => In_The_Items (Items_Index),
               Result        => Pattern_Matched,
               Nested_Status => Is_Match_Status);

            case Is_Match_Status is
               when Illegal_Pattern =>
                  Alogs.Log
                    (Log_ID  => "A39564A08A0178D7",
                     Message => "Illegal_Pattern: Location_Of failed");
                  Booch_Status := Is_Match_Status;
                  return;

               when OK =>
                  null;

            end case;

            if Pattern_Matched then
               Items_Index := Index'Succ (Items_Index);
            else
               Booch_Status := Pattern_Not_Found;
               return;
            end if;

         end loop;

      exception
         when Constraint_Error =>
            Alogs.Status_Exception
              (Log_ID  => "10DFEF66791A5B95",
               Message =>
                 "Constraint_Error: Pattern_Not_Found: Location_Of failed");
            Booch_Status := Exception_Underflow;
            return;

      end Location_Of;

   begin

      declare
         Preprocess_Status : Locus.Preprocess;
      begin
         Preprocess (The_Pattern, Full_Pattern, Preprocess_Status);

         case Preprocess_Status is
            when Illegal_Pattern =>
               Alogs.Log
                 (Log_ID  => "4941C825FA1C88D5",
                  Message => "Illegal_Pattern: Location_Of failed");
               Booch_Status := Preprocess_Status;
               Result := Index'Last;
               return;

            when OK =>
               null;
         end case;
      end;

      for Start in In_The_Items'Range loop
         begin
            Location_Of
              (Full_Pattern, In_The_Items, Start, Result, Booch_Status);

            case Booch_Status is
               when Illegal_Pattern =>
                  Alogs.Log
                    (Log_ID  => "A72231E83628645C",
                     Message => "Illegal_Pattern: Location_Of failed");
                  return;

               when Pattern_Not_Found =>
                  null;

               when OK =>
                  return;

            end case;
         end;
      end loop;
   end Location_Of;

end Booch_Light.Pattern_Match_Regular_Expression;

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
