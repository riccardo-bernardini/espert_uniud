with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Exceptions;

with String_Formatting;

with DVAccum.Config;
with DVAccum.Event_Io;
with DVAccum.Event_Processing;
with DVAccum.Timestamps;
with DVAccum.Frame_Name_Generators;

with DVAccum.Split_Filename;
with Dvaccum.Filters;

use Ada;

procedure DVAccum.Main is
   use type Config.Parsing_Status;

   Bad_Parameters : exception;

   function Source_Filename (N : Positive) return String
     with
       Pre =>
         Config.Package_Ready
         and then N <= Config.N_Inputs;

   function Time_Offset (N : Positive) return Timestamps.Duration
     with
       Pre =>
         Config.Package_Ready
         and then N <= Config.N_Inputs;


   function Source_Filename (N : Positive) return String
   is
      F : constant String := Config.Input_Filename (N);
   begin
      if F = "-" then
         return "/dev/stdin";
      else
         return Split_Filename.True_Filename (F);
      end if;
   end Source_Filename;

   function Time_Offset (N : Positive) return Timestamps.Duration
   is (Split_Filename.Offset_Of (Config.Input_Filename (N)));

   Report : Config.Parsing_Report;
begin
   Config.Parse_Command_Line (Report);

   case Report.Status is
      when Config.Success =>
         null;

      when Config.Bad_Command_Line =>
         Put_Line (Standard_Error,
                   "CLI Error: " & To_String (Report.Message));

         Put_Line (Standard_Error, Config.Short_Help_Text);

         Command_Line.Set_Exit_Status (Command_Line.Failure);
         return;

      when Config.Full_Help_Asked =>

         Put_Line (Standard_Error, Config.Long_Help_Text);

         Command_Line.Set_Exit_Status (Command_Line.Success);
         return;

   end case;

   pragma Assert (Config.Package_Ready);

   declare
      use Frame_Name_Generators;
      use type Timestamps.Duration;

      Start_Time : Timestamps.Timestamp := Config.Start_At;
      Stop_Time : Timestamps.Timestamp := Config.Stop_At;

      Fine_Sampling : constant Timestamps.Duration :=
                        Config.Frame_Period / Float (Config.Oversampling);

      Filter : constant Filters.Filter_Type :=
                 Filters.Parse (Descr    => Config.Filter_Description,
                                Sampling => Timestamps.To_Seconds (Fine_Sampling));

      Frame_Filename_Format : constant String_Formatting.Parsed_Format :=
                                String_Formatting.Parse_Format (Config.Frame_Filename_Template);

      Sequences : array (1 .. Config.N_Inputs) of Event_Io.Event_Sequence;
      Input : File_Type;
   begin
      for N in Sequences'Range loop
         Open
           (File => Input,
            Mode => In_File,
            Name => Source_Filename (N));

         Event_Io.Read_CSV_Event_Stream
           (Input             => Input,
            Events            => Sequences (N),
            On_Positive_Event => Config.Positive_Event_Weight,
            On_Negative_Event => Config.Negative_Event_Weight,
            Offset            => Time_Offset (N));

         Put_Line (N'Image
                   & "  " & Timestamps.Image (Stop_Time)
                   & ", " & Timestamps.Image (Start_Time)
                   & "  " & Timestamps.Image (Sequences (N).First_Time)
                   & ", " & Timestamps.Image (Sequences (N).Last_Time));

         Start_Time := Timestamps.Max (Start_Time, Sequences (N).First_Time);
         Stop_Time  := Timestamps.Min (Stop_Time,  Sequences (N).Last_Time);

         Put_Line (N'Image
                   & "  " & Timestamps.Image (Stop_Time)
                   & ", " & Timestamps.Image (Start_Time));
      end loop;

      if Config.Frame_Period > Stop_Time - Start_Time then
         Put_Line (Timestamps.Image (Stop_Time) & " - " & Timestamps.Image (Start_Time)
                   & "="& Timestamps.Image (Stop_Time - Start_Time)
                   & "<" & Timestamps.Image (Config.Frame_Period));

         raise Bad_Parameters
           with "Not enough data to make a frame";
      end if;

      Put_Line ("from  " & Timestamps.Image (start_Time)
                & " to " & Timestamps.Image (Stop_Time));

      Put_Line ("delta=  " & Timestamps.Image (Stop_Time - Start_Time));

      for N in Sequences'Range loop
         Event_Processing.Process
           (Event_Sequence => Sequences (N),
            Frame_Name     => New_Generator (Format   => Frame_Filename_Format,
                                             Basename => Source_Filename (N)),
            Event_Weight   => Config.Event_Contribution,
            Offset         => 0.0,
            Filter         => Filter,
            From           => Start_Time,
            To             => Stop_Time,
            Frame_Duration => Config.Frame_Period,
            Oversampling   => Config.Oversampling,
            Initial_Image  => Config.Initial_Image
              (Size_X  => Sequences (N).N_Cols,
               Size_Y  => Sequences (N).N_Rows));
      end loop;
   end;

   Command_Line.Set_Exit_Status (Command_Line.Success);
exception
   when E : Event_Io.Bad_Event_Stream =>
      Put_Line (Exceptions.Exception_Message (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when E : Bad_Parameters =>
      Put_Line (Exceptions.Exception_Message (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end DVAccum.Main;
