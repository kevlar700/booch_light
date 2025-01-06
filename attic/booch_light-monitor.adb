--           Original Booch Components (Ada 83 version)
--  Copyright (C) 1987 Grady Booch, provided WITHOUT ANY WARRANTY. Further
--  license details should appear at the end of this file.
--
--  The book SOFTWARE COMPONENTS WITH Ada Structures, Tools, and_Subsystems,
--  ISBN 0-8053-0609-9 by Grady Booch, fully describes the design and usage
--  of this software.

package body Booch_Light.Monitor is

    task body Kind is
        Number_Of_Readers : Natural := 0;
    begin
        loop
            select
                accept Start (The_Service : in Service) do
                    case The_Service is
                        when Read =>
                            Number_Of_Readers := Number_Of_Readers + 1;
                        when Write =>
                            while Number_Of_Readers > 0 loop
                                accept Stop_Reading;
                                Number_Of_Readers := Number_Of_Readers - 1;
                            end loop;
                    end case;
                end Start;
                if Number_Of_Readers = 0 then
                    accept Stop_Writing;
                end if;
            or
                accept Stop_Reading;
                Number_Of_Readers := Number_Of_Readers - 1;
            or
                terminate;
            end select;
        end loop;
    end Kind;

    procedure Start_Reading (The_Monitor : in Kind) is
    begin
        The_Monitor.Start (Read);
    end Start_Reading;

    procedure Stop_Reading (The_Monitor : in Kind) is
    begin
        The_Monitor.Stop_Reading;
    end Stop_Reading;

    procedure Start_Writing (The_Monitor : in Kind) is
    begin
        The_Monitor.Start (Write);
    end Start_Writing;

    procedure Stop_Writing (The_Monitor : in Kind) is
    begin
        The_Monitor.Stop_Writing;
    end Stop_Writing;

end Booch_Light.Monitor;

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
