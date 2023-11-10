with Event_Sequences;

package Event_Streams is
   procedure Read_Event_Stream
     (Filename               : in     String;
      Use_Absolute_Timestamp : in Boolean;
      Events                 :    out Event_Sequences.Event_Sequence;
      Metadata               :    out Event_Sequences.Metadata_Map);

   procedure Save_Binary_Event_Stream
     (Filename : String;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map);

   procedure Save_CSV_Event_Stream
     (Filename : String;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map);

   procedure Save_Event_Stream
     (Filename : String;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map);


     Bad_Event_Stream : exception;
   end Event_Streams;
