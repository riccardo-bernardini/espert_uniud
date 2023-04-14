with ada.Text_IO;

with Ada.Command_Line;

with Event_Sequences;
with Event_Streams;

use Ada;
use Ada.Text_Io;

procedure Convert is
   Events   : Event_Sequences.Event_Sequence;
   Metadata : Event_Sequences.Metadata_Map;
begin
   if Command_Line.Argument_Count /= 2 then
      Put_Line (Standard_Error,
                "Usage: "
                & Command_Line.Command_Name
                & " input output");

      Command_Line.Set_Exit_Status (Command_Line.Failure);
      return;
   end if;

   Event_Streams.Read_Event_Stream (Filename => Command_Line.Argument (1),
                                    Events   => Events,
                                    Metadata => Metadata);

   Event_Streams.Save_Event_Stream (Filename => Command_Line.Argument (2),
                                    Events   => Events,
                                    Metadata => Metadata);

   Command_Line.Set_Exit_Status (Command_Line.Success);
end Convert;
