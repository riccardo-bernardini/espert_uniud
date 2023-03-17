with Config;
with Camera_Events;
with Event_Sequences;
with Event_Streams;
with Images;

procedure Main is
   procedure Extract_Segment (Segment : out Event_Sequences.Event_Sequence;
                              Events  : in  Event_Sequences.Event_Sequence;
                              From    : in out Positive;
                              To      : in Camera_Events.Timestamp)
   is
   begin
      null;
   end Extract_Segment;
begin
   Config.Parse_Command_Line;

   declare
      use type Camera_Events.Duration;
      use type Camera_Events.Timestamp;

      Events : constant Event_Sequences.Event_Sequence :=
                 Event_Streams.Parse_Event_Stream (Config.Input);

      Status : Images.Image_Type := (if Config.Has_Start_Image then
                                        Images.Load (Config.Start_Image_Filename)
                                     else
                                        Images.Zero (640, 480));

      Current_Time : Camera_Events.Timestamp :=
                       Camera_Events.T (Events.First_Element);

      Current_Index : Positive := Events.First_Index;

      Next_Time : Camera_Events.Timestamp;

      Segment : Event_Sequences.Event_Sequence;

      Frame_Number : Natural := 0;
   begin
      loop
         Next_Time := Current_Time + Config.Sampling_Period;

         Next_Index := Time_To_Index (Events, Next_Time, Current_Index);

         Extract_Segment (Segment => Segment,
                          Events  => Events,
                          From    => Current_Index,
                          To      => Next_Time);

         Images.Save (Filename => Frame_Name (Config.Radix, Frame_Number),
                      Image    => Status);

         Current_Time := Next_Time;

         Frame_Number := Frame_Number + 1;
      end loop;
   end;
end Main;
