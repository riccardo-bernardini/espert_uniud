with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;
with DVAccum.Event_Processing.Accumulator_Tasks;
with DVAccum.Event_Processing.Frame_Makers;
with DVAccum.Config;

with System.Multiprocessors;

use System;
with Ada.Text_IO; use Ada.Text_IO;

package body Dvaccum.Event_Processing is
   type Ev_Access is access Event_Array;

   procedure Process (Event_Sequence : Event_Io.Event_Sequence;
                      Frame_Name     : Frame_Name_Generators.Abstract_Generator'Class;
                      Event_Weight   : Sample_Value;
                      Offset         : Sample_Value;
                      Filter         : Filters.Filter_Type;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type)
   is

      procedure Fill_Storage (Source        : Event_Io.Event_Sequence;
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
         Ev               : Events.Event_Type;
      begin
         for Pos in Source.All_Events loop
            Ev := Event_Io.Element (Pos);

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
         Put_Line("N. Cpu=" & N_Cpu'Image);
         for I in Accumulators'Range loop
            Accumulators (I) := new Accumulator_Tasks.Accumulator (I, Parameters);
         end loop;
      end Accumulate;

      -----------------
      -- Save_Frames --
      -----------------

      procedure Save_Frames (Frame_Name : Frame_Name_Generators.Abstract_Generator'Class;
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
                                              Frame_Name    => Generator_Holders.To_Holder (Frame_Name),
                                              Offset        => Offset,
                                              Initial_Image => Initial_Image);
      begin
         for I in Makers'Range loop
            Makers (I) := new Frame_Maker (I, Parameters);
         end loop;
      end Save_Frames;

      Event_Storage : constant Ev_Access :=
                        new Event_Array (1 .. Integer (Event_Sequence.Length));

      Segments : constant Segment_Queues.Segment_Queue_Access :=
                   new Segment_Queues.Segment_Queue;

   begin
      Fill_Storage (Source      => Event_Sequence,
                    Destination => Event_Storage,
                    Segments    => Segments);

      declare
         --
         --  We need to find the number of frames that we are going to
         --  generate.  Let D=Tstop-Tstart be the difference between the
         --  start and stop timestamps.  Let also F the duration of a frame
         --  and let S = F/M the oversampled sampling period (with M the
         --  oversampling factor).
         --
         --  Let u: Z -> R be the signal to be accumulated. Sample
         --  u(n) has the contributions of the events happening in the
         --  interval I_n=[n*S, (n+1)*S).  Therefore, in order to have
         --  u(n) /= 0 the intersection of I_n with [0, D] must be not empty
         --  It is empty if it happend
         --
         --      (n+1)*S <= 0  or n*S > D
         --
         --  Therefore, it is not empty if
         --
         --       (n+1)*S > 0  and n*S <= D
         --
         --  which is equivalent to
         --
         --    -1 < n <= D/S     or equivalently 0 <= n <= floor(D/S)=L
         --
         --  If m is the number of a frame, it must be
         --
         --      0 <= M * m <= floor (D / S), that is 0 <= m <= floor(L/M)
         --
         --  Fine_Sampling : constant Timestamps.Duration :=
         --                    Frame_Duration / Float (Oversampling);

         D : constant Timestamps.Duration := To - From;
         --  L : constant Float := Float'Floor (D / Fine_Sampling);
         N_Frames : constant Positive :=
                      Positive (Float'Floor (D / Frame_Duration)) + 1;

         Pixels : constant Pixel_Buffers.Pixel_Buffer_Access :=
                    Pixel_Buffers.Create (N_Frames => N_Frames,
                                          N_Pixels => Integer (Segments.Size));

      begin
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
      end;
   end Process;
end Dvaccum.Event_Processing;
