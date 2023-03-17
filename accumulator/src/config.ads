with Ada.Text_IO;
with Camera_Events;
with Memory_Dynamic;

package Config is
   procedure Parse_Command_Line;

   Input : Ada.Text_IO.File_Access;

   Sampling_Period : Camera_Event.Duration;

   Time_Constant : Memory_Dynamic.Time_Constant_Type;
end Config;
