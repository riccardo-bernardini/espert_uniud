with Ada.IO_Exceptions;
with Config;
with Camera_Events;
with Event_Sequences;
with Event_Streams;
with Images;
with Memory_Dynamic;
with Ada.Command_Line;

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with Ada.Strings.Fixed;

with Profiling;

procedure Main is
   package Float_Formatting is new Float_IO (Float);

   type Program_Sections is (Parse_Stream, Extract, Collect, Fill, Update, Save);

   package My_Profiler is
     new Profiling (Program_Sections);

   use type Camera_Events.Timestamp;
   use type Ada.Containers.Count_Type;

   function "<=" (A, B : Camera_Events.Timestamp) return Boolean
   is
   begin
      return  not (A > B);
   end "<=";
   pragma Unreferenced ("<=");

   procedure Extract_Segment (Segment : out Event_Sequences.Event_Sequence;
                              Events  : in out Event_Sequences.Event_Sequence;
                              From    : in Camera_Events.Timestamp;
                              To      : in Camera_Events.Timestamp)
     with
       Pre => From < To,
       Post =>
         (Events.Is_Empty or else Camera_Events.T (Events.First_Element) >= To)
         and
           (Segment.Is_Empty or else Camera_Events.T (Segment.Last_Element) < To)
           and
             (Segment.Length + Events.Length = Events.Length'Old);

   procedure Extract_Segment (Segment : out Event_Sequences.Event_Sequence;
                              Events  : in out Event_Sequences.Event_Sequence;
                              From    : in Camera_Events.Timestamp;
                              To      : in Camera_Events.Timestamp)
   is
      use Camera_Events;
   begin
      Segment.Clear;

      while not Events.Is_Empty and then T (Events.First_Element) < To loop
         if T (Events.First_Element) >= From then
            Segment.Append (Events.First_Element);
         end if;

         Events.Delete_First;
      end loop;
   end Extract_Segment;

   procedure Update_Pixel (Start  : Camera_Events.Timestamp;
                           Pixel  : in out Images.Pixel_Value;
                           Events : Event_Sequences.Event_Sequence)
   is
      use Camera_Events;
      use Images;

      Current_Time : Timestamp := Start;
   begin
      --  Put_Line ("IN " & Pixel'Image);

      for Ev of Events loop
         --  Put_Line ("++" & Image (T (Ev)) & Image (Current_Time));
         Pixel := Memory_Dynamic.Evolve (Start   => Pixel,
                                         Dynamic => Config.Forgetting_Method,
                                         Delta_T => T (Ev) - Current_Time);

         Pixel := Pixel + Pixel_Value (Weight (Ev));

         Current_Time := T (Ev);
      end loop;

      --  Put_Line ("OUT " & Pixel'Image);

   end Update_Pixel;

   procedure Put_Line_Maybe (Verbose : Boolean;
                             Text    : String)
   is
   begin
      if Verbose then
         Put_Line (Standard_Error, Text);
      end if;
   end Put_Line_Maybe;

   procedure Put_Maybe (Verbose : Boolean;
                        Text    : String)
   is
   begin
      if Verbose then
         Put (Standard_Error, Text);
         Flush (Standard_Error);
      end if;
   end Put_Maybe;

   procedure Show_Progress_Bar (Start_Time   : Camera_Events.Timestamp;
                                Stop_Time    : Camera_Events.Timestamp;
                                Current_Time : Camera_Events.Timestamp)
   is
      use Camera_Events;
      use Ada.Strings.Fixed;

      Full : constant Camera_Events.Duration := Stop_Time - Start_Time;
      Done : constant Camera_Events.Duration := Current_Time - Start_Time;
      Fraction : constant Float := Done / Full;

      N_Columns : constant Integer := 80;

      Done_Section_Length : constant Integer :=
                              Integer (Fraction * Float (N_Columns));

      Remaining_Section_Length : constant Integer :=
                                   N_Columns - Done_Section_Length - 1;
   begin
      Put (ASCII.CR);
      Put (Done_Section_Length * "=");
      Put (">");

      if Remaining_Section_Length > 0 then
         Put (Remaining_Section_Length * '-');
      end if;

      Put (" ");
      Float_Formatting.Put (Item => 100.0 * Fraction,
                            Fore => 3,
                            Aft  => 1,
                            Exp  => 0);

      Put ("%");
      Flush;
   end Show_Progress_Bar;

   Events   : Event_Sequences.Event_Sequence;
   Metadata : Event_Sequences.Metadata_Map;

   Profiler : My_Profiler.Profiler_Type;
begin
   Config.Parse_Command_Line;

   Put_Maybe (Config.Verbose, "Reading event list...");
   Profiler.Entering (Parse_Stream);

   Event_Streams.Read_Event_Stream (Filename => Config.Input,
                                    Events   => Events,
                                    Metadata => Metadata);


   Put_Line_Maybe (Config.Verbose, " Done");

   Put_Line_Maybe (Config.Verbose, "Size X = N. col =" & Metadata.Size_X'Image);

   Put_Line_Maybe (Config.Verbose, "Size Y = N. row =" & Metadata.Size_Y'Image);

   if Events.Is_Empty then
      Put_Line (Standard_Error, "Empty event stream");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      use type Camera_Events.Duration;
      use type Camera_Events.X_Coordinate_Type;
      use type Camera_Events.Y_Coordinate_Type;
      use type Config.Frame_Index;

      Start_Time : constant Camera_Events.Timestamp :=
                     Config.Start_At (Event_Sequences.T_Min (Events));

      Stopping_Time : constant Camera_Events.Timestamp :=
                        Config.Stop_At (Event_Sequences.T_Max (Events));

      Current_Time : Camera_Events.Timestamp := Start_Time;
      Next_Time    : Camera_Events.Timestamp;


      Current_Frame : Images.Image_Type :=
                        Config.Start_Image (Metadata.Size_X, Metadata.Size_Y);

      Frame_Number : Config.Frame_Index := 0;

      Segment      : Event_Sequences.Event_Sequence;

      Events_At    : Event_Sequences.Point_Event_Map :=
                       Event_Sequences.Create (Metadata.Size_X, Metadata.Size_Y);
   begin
      pragma Assert (Camera_Events.Is_Finite (Start_Time));
      pragma Assert (Camera_Events.Is_Finite (Stopping_Time));
      pragma Assert (Start_Time < Stopping_Time);

      --  Put_Line (Camera_Events.Image (Start_Time) & " .. " & Camera_Events.Image (Stopping_Time));

      while Current_Time < Stopping_Time loop

         if Config.Show_Progress_Bar then
            Show_Progress_Bar (Start_Time, Stopping_Time, Current_Time);
         end if;

         Next_Time := Current_Time + Config.Sampling_Period;

         if Next_Time > Stopping_Time then
            Next_Time := Stopping_Time;
         end if;

         --  Put_Line (Camera_Events.Image (Current_Time) & Camera_Events.Image (Next_Time));
         --  Put_Line (Events.Length'Image);

         Profiler.Entering (Extract);

         Extract_Segment (Segment => Segment,
                          Events  => Events,
                          From    => Current_Time,
                          To      => Next_Time);

         Profiler.Entering (Collect);

         Event_Sequences.Collect_By_Point (Events         => Segment,
                                           Last_Timestamp => Next_Time,
                                           Result         => Events_At);

         --  Profiler.Entering (Fill);
         --
         --  Event_Sequences.Fill_Frame (Events_At,
         --                              Next_Time,
         --                              Metadata.Size_X,
         --                              Metadata.Size_Y);

         Profiler.Entering (Update);

         for Pos in Events_At.Iterate loop
            declare
               Pixel : constant Camera_Events.Point_Type :=
                         Event_Sequences.Point (Pos);
            begin
               --  Put_Line (Pixel.X'Image & Pixel.Y'Image);

               Update_Pixel (Start  => Current_Time,
                             Pixel  => Current_Frame (Pixel.X, Pixel.Y),
                             Events => Events_At (Pixel));
            end;
         end loop;

         Profiler.Entering (Save);

         Images.Save (Filename => Config.Frame_Filename (Frame_Number),
                      Image    => Current_Frame,
                      Format   => Config.Output_Format);

         Current_Time := Next_Time;

         Frame_Number := Frame_Number + 1;
      end loop;

      if Config.Show_Progress_Bar then
         New_Line;
      end if;

      Profiler.Dump;
   end;
exception
   when E : Config.Bad_Command_Line =>
      Put_Line (Standard_Error, Exception_Message (E));
      New_Line (Standard_Error);

      Put_Line (Standard_Error, Config.Short_Help_Text);
      New_Line (Standard_Error);

      Put_Line (Standard_Error,
                "Use " & Ada.Command_Line.Command_Name & " --help for more help");
      New_Line (Standard_Error);

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Event_Streams.Bad_Event_Stream =>
      Put_Line (Standard_Error, "Erroqqqqqqqqqr while parsing event stream:"
                & Exception_Message (E));

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Config.Full_Help_Asked =>
      Put_Line (Standard_Error, Config.Long_Help_Text);

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

   when E :  ADA.IO_EXCEPTIONS.NAME_ERROR =>
      Put_Line (Standard_Error, Exception_Message (E));

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Main;
