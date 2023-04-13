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

procedure Main is
   use type Camera_Events.Timestamp;
   use type Ada.Containers.Count_Type;

   function "<=" (A, B : Camera_Events.Timestamp) return Boolean
   is
   begin
      return  not (A > B);
   end "<=";

   procedure Extract_Segment (Segment : out Event_Sequences.Event_Sequence;
                              Events  : in out Event_Sequences.Event_Sequence;
                              To      : in Camera_Events.Timestamp)
     with
       Pre => not Events.Is_Empty,
       Post =>
         (Events.Is_Empty or else Camera_Events.T (Events.First_Element) > To)
         and
           (Segment.Is_Empty or else Camera_Events.T (Segment.Last_Element) <= To)
           and
             (Segment.Length + Events.Length = Events.Length'Old);

   procedure Extract_Segment (Segment : out Event_Sequences.Event_Sequence;
                              Events  : in out Event_Sequences.Event_Sequence;
                              To      : in Camera_Events.Timestamp)
   is
      use Camera_Events;
   begin
      Segment.Clear;

      while not Events.Is_Empty and then T (Events.First_Element) <= To loop
         Segment.Append (Events.First_Element);
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
      for Ev of Events loop
         Pixel := Memory_Dynamic.Evolve (Start   => Pixel,
                                         Dynamic => Config.Forgetting_Method,
                                         Delta_T => T (Ev) - Current_Time);

         Pixel := Pixel + Pixel_Value (Weight (Ev));

         Current_Time := T (Ev);
      end loop;
   end Update_Pixel;

begin
   Config.Parse_Command_Line;

   declare
      use type Camera_Events.Duration;
      use type Config.Frame_Index;


      Current_Time : Camera_Events.Timestamp;

      Next_Time  : Camera_Events.Timestamp;

      Segment : Event_Sequences.Event_Sequence;

      Frame_Number : Config.Frame_Index := 0;

      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map;
   begin
      Event_Streams.Parse_Event_Stream (Input    => Config.Input.all,
                                        Events   => Events,
                                        Metadata => Metadata);

      Current_Time := Camera_Events.T (Events.First_Element);

      declare
         Current_Frame : Images.Image_Type :=
                           Config.Start_Image (Metadata.Size_X, Metadata.Size_Y);
      begin
         while not Events.Is_Empty loop
            Next_Time := Current_Time + Config.Sampling_Period;


            Extract_Segment (Segment => Segment,
                             Events  => Events,
                             To      => Next_Time);

            declare
               use Event_Sequences;

               Events_At : constant Point_Event_Map :=
                             Collect_By_Point (Segment, Next_Time);
            begin
               for X in Events_At'Range (1) loop
                  for Y in Events_At'Range (2) loop
                     Update_Pixel (Start => Current_Time,
                                   Pixel  => Current_Frame (X, Y),
                                   Events => Events_At (X, Y));
                  end loop;
               end loop;
            end;

            Images.Save (Filename => Config.Frame_Filename (Frame_Number),
                         Image    => Current_Frame,
                         Format   => Config.Output_Format);

            Current_Time := Next_Time;

            Frame_Number := Frame_Number + 1;
         end loop;
      end;
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
      Put_Line (Standard_Error, "Error while parsing event stream:"
                & Exception_Message (E));

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Config.Full_Help_Asked =>
      Put_Line (Standard_Error, Config.Long_Help_Text);

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

   when E :  ADA.IO_EXCEPTIONS.NAME_ERROR =>
      Put_Line (Standard_Error, Exception_Message (E));

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Main;
