with Ada.Text_IO;
with Event_Sequences;

package Event_Streams is
   procedure Parse_Event_Stream
     (Input    : in     Ada.Text_Io.File_Type;
      Events   :    out Event_Sequences.Event_Sequence;
      Metadata :    out Event_Sequences.Metadata_Map);

   Bad_Event_Stream : exception;
end Event_Streams;
