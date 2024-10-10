with Ada.Containers.Doubly_Linked_Lists;

with DVAccum.Events;

private package DVaccum.Event_Processing.Segment_Queues is
      subtype Event_Index is Positive;

   type Event_Array is array(Event_Index range <>) of Events.Event_Type;

   type Event_Array_Access is access Event_Array;

   type Event_Segment is
      record
         First : Event_Index;
         Last  : Event_Index;
      end record;

   No_Segment : constant Event_Segment := (First => 2, Last => 1);

   package Segment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Event_Segment);

   protected type Segment_Queue is
      entry Unused_Segment (S : out Event_Segment);

      procedure Append (S : Event_Segment);
   private
      Segments : Segment_Lists.List;
   end Segment_Queue;

   type Segment_Queue_Access is access Segment_Queue;
end DVaccum.Event_Processing.Segment_Queues;
