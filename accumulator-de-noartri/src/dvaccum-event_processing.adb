with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;

with System.Multiprocessors;


package body Dvaccum.Event_Processing is
   procedure Process (Event_Sequence : Event_Io.Event_Sequences.Set;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Float;
                      Filer          : Filter_Spec;
                      Time_Origin    : Timestamps.Timestamp;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type)
   is
      type Ev_Access is access Event_Array;

      procedure Fill_Storage (Source        : Event_Io.Event_Sequences.Set;
                              Destination   : Ev_Access;
                              Segments      : Segment_Queues.Segment_Queue_Access)
      is
         use Segment_Queues;
         use Frames;

         No_Pixel : constant Frames.Point_Type := (X_Coordinate_Type'Last,
                                                   Y_Coordinate_Type'Last);

         Current_Pixel : Frames.Point_Type := No_Pixel;

         Cursor   : Event_Index := Destination'First;
         Begin_Of_Segment : Event_Index;
      begin
         for Ev of Source loop
            if Current_Pixel = No_Pixel then
               Begin_Of_Segment := Cursor;
               Current_Pixel := (Events.X (Ev), Events.Y (Ev));

            elsif Current_Pixel /= (Events.X (Ev), Events.Y (Ev)) then
               Segments.Append (Event_Segment'(First    => Begin_Of_Segment,
                                               Last     => Cursor - 1,
                                               Location => Current_Pixel));

               Begin_Of_Segment := Cursor;
               Current_Pixel := (Events.X (Ev), Events.Y (Ev));

            end if;

            Destination (Cursor) := Ev;
            Cursor := Cursor + 1;
         end loop;

         if Current_Pixel /= No_Pixel then
            Segments.Append (Event_Segment'(First    => Begin_Of_Segment,
                                            Last     => Cursor - 1,
                                            Location => Current_Pixel));
         end if;

         Segments.Done_Appending;
      end Fill_Storage;

      Event_Storage : constant Ev_Access :=
                        new Event_Array (1 .. Integer (Event_Sequence.Length));

      Segments : constant Segment_Queues.Segment_Queue_Access :=
                   new Segment_Queues.Segment_Queue;

      N_Cpu : constant System.Multiprocessors.CPU :=
                System.Multiprocessors.Number_Of_CPUs;

      Accumulators : Accumulator_Array (1 .. N_Cpu);
   begin
      Fill_Storage (Source      => Event_Sequence,
                    Destination => Event_Storage,
                    Segments    => Segments);


   end Process;
end Dvaccum.Event_Processing;
