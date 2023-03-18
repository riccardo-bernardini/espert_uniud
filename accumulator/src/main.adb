with Config;
with Camera_Events;
with Event_Sequences;
with Event_Streams;
with Images;

procedure Main is
   use type Event_Sequences.Event_Index;
   use type Camera_Events.Timestamp;

   procedure Extract_Segment (Segment : out Event_Sequences.Event_Sequence;
                              Events  : in  Event_Sequences.Event_Sequence;
                              From    : in out Event_Sequences.Event_Index;
                              To      : in Event_Sequences.Event_Index)
   is
   begin
      raise Program_Error;
   end Extract_Segment;

   procedure Update_Pixel (Current_Time : Camera_Events.Timestamp;
                           Pixel        : in out Images.Pixel_Value;
                           Events       : Event_Sequences.Event_Sequence)
   is
      use Camera_Events;
   begin
      raise Program_Error;
   end Update_Pixel;

   function Time_To_Index (Events     : Event_Sequences.Event_Sequence;
                           Timestamp  : Camera_Events.Timestamp;
                           Start_From : Event_Sequences.Event_Index)
                           return Event_Sequences.Event_Index
   is

   begin
      for I in Start_From .. Events.Last_Index loop
         if Camera_Events.T (Events (I)) > Timestamp then
            return I;
         end if;
      end loop;

      return Events.Last_Index + 1;
   end Time_To_Index;
begin
   Config.Parse_Command_Line;

   declare
      use type Camera_Events.Duration;
      use type Camera_Events.Timestamp;
      use type Config.Frame_Index;

      Events : constant Event_Sequences.Event_Sequence :=
                 Event_Streams.Parse_Event_Stream (Config.Input.all);

      Status : Images.Image_Type := Config.Start_Image;

      Current_Time : Camera_Events.Timestamp :=
                       Camera_Events.T (Events.First_Element);

      Current_Index : Event_Sequences.Event_Index := Events.First_Index;

      Next_Index : Event_Sequences.Event_Index;
      Next_Time  : Camera_Events.Timestamp;

      Segment : Event_Sequences.Event_Sequence;

      Frame_Number : Config.Frame_Index := 0;

   begin
      loop
         Next_Time := Current_Time + Config.Sampling_Period;

         Next_Index := Time_To_Index (Events, Next_Time, Current_Index);

         Extract_Segment (Segment => Segment,
                          Events  => Events,
                          From    => Current_Index,
                          To      => Next_Index);

         declare
            Event_By_Point : constant Event_Sequences.Point_Event_Map :=
                               Event_Sequences.Collect_By_Point (Segment, Next_Time);
         begin
            for X in Event_By_Point'Range (1) loop
               for Y in Event_By_Point'Range (2) loop
                  Update_Pixel (Current_Time, Status (X, Y), Event_By_Point (X, Y));
               end loop;
            end loop;
         end;

         Images.Save (Filename => Config.Frame_Filename (Frame_Number),
                      Image    => Status);

         Current_Time := Next_Time;

         Frame_Number := Frame_Number + 1;
      end loop;
   end;
end Main;
