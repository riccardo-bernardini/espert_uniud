
package body Dvaccum.Event_Processing.Accumulator_Tasks is
   task body Accumulator is
      use Segment_Queues;
      use Pixel_Buffers;

      function Subsample (Data : Filters.Signal)
                          return Pixel_History
      is
         Size : constant Positive := Data'Length / Parameters.Oversampling;
      begin
         return Result : Pixel_History (0 .. Size - 1) do
            for I in Result'Range loop
               Result (I) := Data (I * Parameters.Oversampling);
            end loop;
         end return;
      end Subsample;

      procedure Store (Data     : Pixel_History;
                       Location : Point_Type)
      is
      begin
         Parameters.Pixels.Store (Location, Data);
      end Store;

      type Event_Array is
        array (Natural range <>) of Events.Event_Type;

      function Extract_Segment (Start, Stop : Natural)
                                return Event_Array
      is
         use type Timestamps.Timestamp;

         Buffer : Event_Array (0 .. Stop - Start + 1);
         Cursor : Natural := Buffer'First;
      begin
         for I in Start .. Stop loop
            if
              Parameters.Events (I).T >= Parameters.From and
              Parameters.Events (I).T <= Parameters.To
            then
               Buffer (Cursor) := Parameters.Events (I);
               Cursor := Cursor + 1;
            end if;
         end loop;

         return Buffer;
      end Extract_Segment;

      function Collate (Segment : Event_Array) return Filters.Signal
      is
         use Timestamps;

         Step : constant Timestamps.Duration :=
                  Parameters.Frame_Duration / Float (Parameters.Oversampling);

         N_Samples : constant Positive :=
                       Positive ((Parameters.To - Parameters.From) / Step);


         Index : Natural;
      begin
         return Result : Filters.Signal (0 .. N_Samples - 1) := (others => 0.0)
         do
            for Ev of Segment loop
               Index := Natural ((Ev.T - Parameters.From) / Step);

               Result (Index) := Result (Index) +
                 Sample_Value (Integer (Ev.Weight));
            end loop;
         end return;
      end Collate;

      Working_Segment : Event_Segment;

      Filter : Filters.Filter_Type := Parameters.Filter;
   begin
      loop
         Parameters.Segments.Next_Segment (Working_Segment);

         exit when Working_Segment = No_Segment;

         declare
            Segment : constant Event_Array := Extract_Segment (Working_Segment.First,
                                                               Working_Segment.Last);

            Signal    : constant Filters.Signal := Collate (Segment);
            Recovered : constant Filters.Signal := Filters.Apply (Filter, Signal);
            Result    : constant Pixel_History  := Subsample (Recovered);
         begin
            Store (Result, Working_Segment.Location);
         end;
      end loop;
   end Accumulator;
end Dvaccum.Event_Processing.Accumulator_Tasks;
