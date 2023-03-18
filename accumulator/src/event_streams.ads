with Ada.Text_IO;
with Event_Sequences;

package Event_Streams is
   function Parse_Event_Stream (Input : Ada.Text_Io.File_Type)
                                return Event_Sequences.Event_Sequence;
end Event_Streams;
