
package body Dvaccum.Event_Processing.Accumulator_Tasks is
   task body Accumulator is
      use Segment_Queues;

      procedure Store (Data     : Filters.Signal;
                       Location : Frames.Point_Type)
      is
         N_Stored : constant Positive := Data'Length / Parameters.Oversampling;
         Buffer   : Pixel_Buffers.Pixel_History (0 .. N_Stored - 1);
      begin
         for I in Buffer'Range loop
            Buffer (I) := Data (I * Parameters.Oversampling);
         end loop;

         Parameters.Pixels.Store (Location, Buffer);
      end Store;

      function Make_Signal (Start, Stop : Natural)
                            return Filters.Signal
      is
         Buffer : Filters.Signal (0 .. Stop - Start + 1);
         Cursor : Natural := Result'First;
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
      end Make_Signal;

      Working_Segment : Event_Segment;

      Filter : Filters.Filter_Type := Parameters.Filter;
   begin
      loop
         Parameters.Segments.Next_Segment (Working_Segment);

         exit when Working_Segment = No_Segment;

         declare
            Signal : constant Filters.Signal := Make_Signal (Working_Segment.First,
                                                             Working_Segment.Last);
         begin
            Store (Filters.Apply (Filter, Signal), Working_Segment.Location);
         end;
      end loop;
   end Accumulator;
end Dvaccum.Event_Processing.Accumulator_Tasks;
