with Ada.Containers.Doubly_Linked_Lists;

--
--  The events, after being read, are organized in a pixel-wise order
--  that is, events that refer to the same pixel are stored together,
--  with the timestamps in increasing order.  A sequence of events
--  correspoinding to the same pixel is called a *segment*.
--
--  The events are stored in an event array in the aformentioned
--  order.  Every task will take a segment in turn, process it
--  and store the result in a pixel buffer.  The access to the segments
--  is managed by a protected object that allows you to
--
--    1,  Add a new segment
--    2.  Take the next segment to be processed
--

private package DVaccum.Event_Processing.Segment_Queues is

   type Event_Segment is
      record
         First    : Event_Index;
         Last     : Event_Index;
         Location : Point_Type;
      end record;

   No_Segment : constant Event_Segment;

   package Segment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Event_Segment);

   protected type Segment_Queue is
      procedure Append (S : Event_Segment);
      procedure Done_Appending;

      procedure Next_Segment (S : out Event_Segment);

      function Size return Ada.Containers.Count_Type;
   private
      Segments : Segment_Lists.List;
   end Segment_Queue;

   type Segment_Queue_Access is access Segment_Queue;

private
   No_Segment : constant Event_Segment := (First    => 2,
                                           Last     => 1,
                                           Location => (0, 0));

end DVaccum.Event_Processing.Segment_Queues;
