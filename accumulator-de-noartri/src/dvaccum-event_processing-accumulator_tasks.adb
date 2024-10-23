with Ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;

package body Dvaccum.Event_Processing.Accumulator_Tasks is
   task body Accumulator is
      use Segment_Queues;
      use Pixel_Buffers;

      procedure Stampa (X : String) is
      begin
         Put_Line ("[" & ID'Image & "]  " & X);
      end Stampa;

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

         Buffer : Event_Array (0 .. Stop - Start + 1);
         Cursor : Natural := Buffer'First;
      begin
         Stampa ("EXTRACT from:" & Timestamps.Image (Parameters.From)
                 & " to:" & Timestamps.Image (Parameters.To)
                 & "start, stop: " & Start'Image& " " & Stop'Image);

         for I in Start .. Stop loop
            Stampa ("EXTRACT  I=" & I'Image & " "& Parameters.Events (I).Image);

            if
              Parameters.Events (I).T >= Parameters.From and
              Parameters.Events (I).T <= Parameters.To
            then
               Stampa ("EXTRACT " & I'Image & "->" & Parameters.Events (I).Image);

               Buffer (Cursor) := Parameters.Events (I);
               Cursor := Cursor + 1;
            end if;
         end loop;

         return Buffer (Buffer'First .. Cursor - 1);
      end Extract_Segment;

      function Collate (Segment : Event_Array) return Filters.Signal
      is
         Step : constant Timestamps.Duration :=
                  Parameters.Frame_Duration / Float (Parameters.Oversampling);

         Last_Time : constant Positive :=
                       Positive (Float'Floor ((Parameters.To - Parameters.From) / Step));


         Index : Natural;
      begin

         return Result : Filters.Signal (0 .. Last_Time) := (others => 0.0)
         do
            for Ev of Segment loop
               Stampa (Ev.Image);
               Stampa ("b3" & Timestamps.Image (Ev.T) & ", " & Timestamps.Image (Parameters.From));

               Index := Natural (Float'Floor ((Ev.T - Parameters.From) / Step));

               Stampa (Index'Image & " " & Result'Last'Image);

               Result (Index) := Result (Index) +
                 Sample_Value (Integer (Ev.Weight));
            end loop;
         end return;
      end Collate;

      Working_Segment : Event_Segment;

      Filter : Filters.Filter_Type := Parameters.Filter;
   begin
      Stampa("a1");
      loop

      Stampa("a2");
         Parameters.Segments.Next_Segment (Working_Segment);

      Stampa("a3");
         exit when Working_Segment = No_Segment;

         declare
            Segment : constant Event_Array := Extract_Segment (Working_Segment.First,
                                                               Working_Segment.Last);

            Signal    : constant Filters.Signal := Collate (Segment);
            Recovered : constant Filters.Signal := Filters.Apply (Filter, Signal);
            Result    : constant Pixel_History  := Subsample (Recovered);
         begin

      Stampa("a4");
            Store (Result, Working_Segment.Location);
         end;
      end loop;
   exception
      when E : others =>
         Stampa ("Bum!" & Ada.Exceptions.Exception_Information(E));
   end Accumulator;
end Dvaccum.Event_Processing.Accumulator_Tasks;
