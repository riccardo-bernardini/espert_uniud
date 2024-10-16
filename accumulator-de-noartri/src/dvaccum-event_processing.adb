with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;
with DVAccum.Event_Processing.Accumulator_Tasks;
with DVAccum.Event_Processing.Frame_Makers;
with DVAccum.Config;

with System.Multiprocessors;

use System;

package body Dvaccum.Event_Processing is
   type Ev_Access is access Event_Array;

   procedure Process (Event_Sequence : Event_Io.Event_Sequences.Set;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Sample_Value;
                      Offset         : Sample_Value;
                      Filter         : Filters.Filter_Type;
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

         function Pixel_Of (Ev : Events.Event_Type) return Point_Type
         is ((Ev.X, Ev.Y));
         No_Pixel : constant Point_Type := (Coord_X'Last,
                                            Coord_Y'Last);

         Current_Pixel : Point_Type := No_Pixel;

         Cursor           : Event_Index := Destination'First;
         Begin_Of_Segment : Event_Index;
      begin
         for Ev of Source loop
            if Current_Pixel = No_Pixel then
               Begin_Of_Segment := Cursor;
               Current_Pixel := Pixel_Of (Ev);

            elsif Current_Pixel /= Pixel_Of(Ev) then
               Segments.Append (Event_Segment'(First    => Begin_Of_Segment,
                                               Last     => Cursor - 1,
                                               Location => Current_Pixel));

               Begin_Of_Segment := Cursor;
               Current_Pixel := Pixel_Of (Ev);

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
         Pixels        : Pixel_Buffers.Pixel_Buffer_Access;
         Filter        : Filters.Filter_Type;
         Event_Weight  : Sample_Value;
         N_Cpu         : System.Multiprocessors.CPU)
      is
         use System.Multiprocessors;
         use Filters;

         Scaled_Filter : constant Filters.Filter_Type := Event_Weight * Filter;

         type Accumulator_Access is
           access Accumulator_Tasks.Accumulator;

         type Accumulator_Array is
           array (CPU range <>) of Accumulator_Access;

         Accumulators : Accumulator_Array (1 .. N_Cpu);

         Parameters   : constant Accumulator_Tasks.Parameter_Access :=
                          new Accumulator_Tasks.Parameter_Record'
                            (Segments        => Segments,
                             Events          => Event_Array_Access (Event_Storage),
                             Pixels          => Pixels,
                             Filter          => Scaled_Filter,
                             From            => From,
                             To              => To,
                             Frame_Duration  => Frame_Duration,
                             Oversampling    => Oversampling);
      begin
         for I in Accumulators'Range loop
            Accumulators (I) := new Accumulator_Tasks.Accumulator (Parameters);
         end loop;
      end Accumulate;

      -----------------
      -- Save_Frames --
      -----------------

      procedure Save_Frames (Frame_Name : Frame_Name_Generator;
                             Pixels     : Pixel_Buffers.Pixel_Buffer_Access;
                             Offset     : Sample_Value;
                             N_Cpu      : System.Multiprocessors.CPU)
      is
         -- use System.Multiprocessors;
         use Frame_Makers;

         type Maker_Access is
           access Frame_Maker;

         type Maker_Array is
           array (Multiprocessors.CPU range <>) of Maker_Access;

         Makers : Maker_Array (1 .. N_Cpu);

         Parameters : constant Parameter_Access :=
                        new Parameter_Record'(Last_X        => Initial_Image'Last (1),
                                              Last_Y        => Initial_Image'Last (2),
                                              Pixels        => Pixels,
                                              Frame_Name    => Frame_Name,
                                              Offset        => Offset,
                                              Initial_Image => Initial_Image);
      begin
         for I in Makers'Range loop
            Makers (I) := new Frame_Maker (Parameters);
         end loop;
      end Save_Frames;

      Event_Storage : constant Ev_Access :=
                        new Event_Array (1 .. Integer (Event_Sequence.Length));

      Segments : constant Segment_Queues.Segment_Queue_Access :=
                   new Segment_Queues.Segment_Queue;

      Pixels : constant Pixel_Buffers.Pixel_Buffer_Access :=
                 Pixel_Buffers.Create (-1, -1);


   begin
      Fill_Storage (Source      => Event_Sequence,
                    Destination => Event_Storage,
                    Segments    => Segments);

      Accumulate (Event_Storage => Event_Storage,
                  Segments      => Segments,
                  Pixels        => Pixels,
                  Filter        => Filter,
                  Event_Weight  => Event_Weight,
                  N_Cpu         => Config.Number_Of_Parallel_Tasks);

      Save_Frames (Frame_Name => Frame_Name,
                   Pixels     => Pixels,
                   Offset     => Offset,
                   N_Cpu      => Config.Number_Of_Parallel_Tasks);
   end Process;
end Dvaccum.Event_Processing;
