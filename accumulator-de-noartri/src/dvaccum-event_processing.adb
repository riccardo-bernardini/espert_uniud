with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;
with DVAccum.Event_Processing.Accumulator_Tasks;
with DVAccum.Event_Processing.Frame_Makers;
--  with DVAccum.Event_Processing.Countdowns;

with System.Multiprocessors;


package body Dvaccum.Event_Processing is
   type Ev_Access is access Event_Array;

   procedure Process (Event_Sequence : Event_Io.Event_Sequences.Set;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Float;
                      Filter         : Filter_Spec;
                      Origin_Shift   : Timestamps.Duration;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type)
   is

      procedure Fill_Storage (Source        : Event_Io.Event_Sequences.Set;
                              Destination   : Ev_Access;
                              Segments      : Segment_Queues.Segment_Queue_Access)
      is
         use Segment_Queues;
         use Frames;

         No_Pixel : constant Frames.Point_Type := (X_Coordinate_Type'Last,
                                                   Y_Coordinate_Type'Last);

         Current_Pixel : Frames.Point_Type := No_Pixel;

         Cursor           : Event_Index := Destination'First;
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

            Destination (Cursor) := Events.Translate (Ev, Origin_Shift);
            Cursor := Cursor + 1;
         end loop;

         if Current_Pixel /= No_Pixel then
            Segments.Append (Event_Segment'(First    => Begin_Of_Segment,
                                            Last     => Cursor - 1,
                                            Location => Current_Pixel));
         end if;

         Segments.Done_Appending;
      end Fill_Storage;

      procedure Accumulate
        (Event_Storage : Ev_Access;
         Segments      : Segment_Queues.Segment_Queue_Access;
         Pixels        : Pixel_Buffers.Pixel_Buffer;
         Filter        : Filter_Spec;
         Event_Weight  : Float;
         N_Cpu         : System.Multiprocessors.CPU)
      is
         use System.Multiprocessors;
         use Frames;

         Scaled_Filter : Filter_Spec := Filter;

         type Accumulator_Access is
           access Accumulator_Tasks.Accumulator;

         type Accumulator_Array is
           array (CPU range <>) of Accumulator_Access;

         Accumulators : Accumulator_Array (1 .. N_Cpu);

         Parameters   : constant Accumulator_Tasks.Parameter_Access :=
                          new Accumulator_Tasks.Parameter_Record'
                            (Num_Degree     => Filter.Num_Degree,
                             Den_Degree     => Filter.Den_Degree,
                             Segments       => Segments,
                             Events         => Event_Array_Access (Event_Storage),
                             Pixels         => Pixels,
                             Filter         => Scaled_Filter,
                             From           => From,
                             To             => To,
                             Frame_Duration  => Frame_Duration,
                             Oversampling   => Oversampling);
      begin
         for Coeff of  Scaled_Filter.Num loop
            Coeff := Coeff * Pixel_Value (Event_Weight);
         end loop;

         for I in Accumulators'Range loop
            Accumulators (I) := new Accumulator_Tasks.Accumulator(Parameters);
         end loop;
      end Accumulate;

      -----------------
      -- Save_Frames --
      -----------------

      procedure Save_Frames (Frame_Name : Frame_Name_Generator;
                             Pixels     : Pixel_Buffers.Pixel_Buffer;
                             N_Cpu      : System.Multiprocessors.CPU)
      is
         use System.Multiprocessors;

         type Maker_Array is
           array (CPU range <>) of Frame_Makers.Frame_Maker;

         Makers : Maker_Array (1 .. N_Cpu);
      begin
         for I in Makers'Range loop
            Makers (I).Start (Pixels        => Pixels,
                              Frame_Name    => Frame_Name,
                              Initial_Image => Initial_Image);
         end loop;
      end Save_Frames;

      Event_Storage : constant Ev_Access :=
                        new Event_Array (1 .. Integer (Event_Sequence.Length));

      Segments : constant Segment_Queues.Segment_Queue_Access :=
                   new Segment_Queues.Segment_Queue;

      Pixels : Pixel_Buffers.Pixel_Buffer;


   begin
      Fill_Storage (Source      => Event_Sequence,
                    Destination => Event_Storage,
                    Segments    => Segments);

      Accumulate (Event_Storage => Event_Storage,
                  Segments      => Segments,
                  Pixels        => Pixels,
                  Filter        => Filter,
                  Event_Weight  => Event_Weight,
                  N_Cpu         => System.Multiprocessors.Number_Of_CPUs);

      Save_Frames (Frame_Name => Frame_Name,
                   Pixels     => Pixels,
                   N_Cpu      => System.Multiprocessors.Number_Of_CPUs);
   end Process;
end Dvaccum.Event_Processing;
