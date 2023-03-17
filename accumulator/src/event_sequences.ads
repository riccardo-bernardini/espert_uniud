with Ada.Containers.Vectors;
with Camera_Events;

package Event_Sequences is
   use type Camera_Events.Event_Type;

   package Event_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Camera_Events.Event_Type);

   subtype Event_Sequence is Event_Vectors.Vector;
end Event_Sequences;
