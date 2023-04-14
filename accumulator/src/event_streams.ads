with Event_Sequences;

package Event_Streams is
   procedure Read_Event_Stream
     (filename    : in     String;
      Events   :    out Event_Sequences.Event_Sequence;
      Metadata    :    out Event_Sequences.Metadata_Map);

   Bad_Event_Stream : exception;
end Event_Streams;
