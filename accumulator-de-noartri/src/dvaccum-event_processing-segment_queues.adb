pragma Ada_2012;
package body DVaccum.Event_Processing.Segment_Queues is

   -------------------
   -- Segment_Queue --
   -------------------

   protected body Segment_Queue is

      ------------
      -- Append --
      ------------

      procedure Append (S : Event_Segment) is
      begin
         Segments.Append (S);
      end Append;

      --------------------
      -- Done_Appending --
      --------------------

      procedure Done_Appending is
      begin
         null;
      end Done_Appending;

      ------------------
      -- Next_Segment --
      ------------------

      procedure Next_Segment (S : out Event_Segment) is

      begin
         if Segments.Is_Empty then
            S := No_Segment;
         else
            S := Segments.First_Element;

            Segments.Delete_First;
         end if;
      end Next_Segment;

      ----------
      -- Size --
      ----------

      function Size return Ada.Containers.Count_Type is
      begin
         return Segments.Length;
      end Size;

   end Segment_Queue;

end DVaccum.Event_Processing.Segment_Queues;
