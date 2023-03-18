with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

with Camera_Events;

package Event_Sequences is
   use type Camera_Events.Event_Type;

   --  type Event_Index is new Positive;

   package Event_Vectors is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Camera_Events.Event_Type);

   use type Event_Vectors.List;

   subtype Event_Sequence is Event_Vectors.List;

   function "<" (A, B : Camera_Events.Point_Type) return Boolean;

   --  package Point_Event_Maps is
   --    new Ada.Containers.Ordered_Maps (Key_Type     => Camera_Events.Point_Type,
   --                                     Element_Type => Event_Sequence);
   --
   --  subtype Point_Event_Map is Point_Event_Maps.Map;

   type Point_Event_Map is
     array (Camera_Events.X_Coordinate_Type range <>,
            Camera_Events.Y_Coordinate_Type range <>)
     of Event_Sequence;

   function Collect_By_Point (Events         : Event_Sequence;
      Last_Timestamp : Camera_Events.Timestamp)
                              return Point_Event_Map;
end Event_Sequences;
