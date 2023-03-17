with Config;
with Camera_Events;
with Event_Sequences;
with Event_Streams;
with Images;

procedure Main is

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

      Next_Time : Camera_Events.Timestamp;
   begin
      loop
         Next_Time := Current_Time + Config.Sampling_Period;
      end loop;
   end;
end Main;
