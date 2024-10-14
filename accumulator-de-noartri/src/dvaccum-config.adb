pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Command_Line;
with Ada.Strings.Fixed;        use Ada.Strings;
with Ada.Characters.Latin_9;

with Sparked_Command_Line;

with Generic_Command_Line_Parser;

with Interfaces.C;

use Interfaces;

with DVAccum.Config.Syntax;  use DVAccum.Config.Syntax;
with DVAccum.Config.Data;    use DVAccum.Config.Data;

with Patterns;
--  with Event_Sequences;
--  with Event_Streams;


package body DVAccum.Config with SPARK_Mode is
   use type Timestamps.Timestamp;

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));

   function Is_A_Tty (Descriptor : C.Int) return Boolean
   is
      use type C.Int;

      function C_Is_A_Tty (Descriptor : C.Int) return C.Int
        with
          Import => True,
          Convention => C,
          External_Name => "isatty",
          Global => (null);
   begin
      return (C_Is_A_Tty (Descriptor) = 1);
   end Is_A_Tty;


   function Package_Ready return Boolean
   is (Data.Is_All_Set);

   ------------------------
   -- Parse_Command_Line --
   ------------------------




   procedure Parse_Command_Line (Report : out Parsing_Report) is
      use Sparked_Command_Line;
      use type Frames.Pixel_Value;

      type Options is
        (
         Filter,
         Oversampling,
         Parallel,
         Frame_Rate,
         Output_Template,
         Input_Spec,
         Log_Progress,
         First_Image,
         Start_Time,
         Stop_Time,
         Min,
         Max,
         Neutral,
         Event_Weigth
        );

      package CL_Parser is
        new Generic_Command_Line_Parser (Options);

      Option_Specs : constant CL_Parser.Option_Names :=
                       (
                        Filter               => +"filter",
                        Oversampling         => +"oversampling|over",
                        Parallel             => +"n-tasks|parallel",
                        Frame_Rate           => +"framerate|frame-rate|fps",
                        Output_Template      => +"output",
                        Input_Spec           => +"input",
                        Log_Progress         => +"log-progress|log-to|progress|log",
                        First_Image          => +"first-image|first",
                        Start_Time           => +"start",
                        Stop_Time            => +"stop",
                        Min                  => +"min",
                        Max                  => +"max",
                        Neutral              => +"neutral",
                        Event_Weigth         => +"weight"
                       );

      Defaults : constant CL_Parser.Option_Defaults :=
                   (
                    Filter               => CL_Parser.Mandatory_Option,
                    Oversampling         => (CL_Parser.Use_Default, +"1"),
                    Parallel             => CL_Parser.Ignore_If_Missing,
                    Frame_Rate           => CL_Parser.Mandatory_Option
                    ,
                    Output_Template      => CL_Parser.Mandatory_Option,
                    Input_Spec           => CL_Parser.Ignore_If_Missing,
                    Log_Progress         => (CL_Parser.Use_Default, +""),
                    First_Image          => (CL_Parser.Use_Default, +""),
                    Start_Time           => CL_Parser.Ignore_If_Missing,
                    Stop_Time            => CL_Parser.Ignore_If_Missing,
                    Min                  => (CL_Parser.Use_Default, +"-1.0"),
                    Max                  => (CL_Parser.Use_Default, +"1.0"),
                    Neutral              => CL_Parser.Ignore_If_Missing,
                    Event_Weigth         => (CL_Parser.Use_Default, +"0.25")
                   );

      procedure Set_Sampling_Spec (Msg : out Unbounded_String)
        with
          Pre => not Is_Set (Sampling_Period),
          Post => Is_Set (Sampling_Period);

      procedure Set_Start_And_Stop_Times
        with
          Pre =>
            not Is_Set (Start_Time)
            and not Is_Set (Stop_Time),
            Post =>
              Is_Set (Start_Time)
              and  Is_Set (Stop_Time);

      procedure Set_Levels
        with
          Pre => not Is_Set (Min)
          and not Is_Set (Max)
          and not Is_Set (Neutral)
          and not Parsed_Options (Min).Missing
          and not Parsed_Options (Max).Missing,
          Post => Is_Set (Min)
          and Is_Set (Max)
          and Is_Set (Neutral);

      --  procedure Set_Decay
      --    with
      --      Pre => not Is_Set (Decay),
      --      Post => Is_Set (Decay);


      function Help_Asked return Boolean
      is (Argument_Count = 0 or else
            (Argument_Count = 1 and then
               (Argument (1) = "help"
                or Argument (1) = "h"
                or Argument (1) = "-h"
                or Argument (1) = "--help"
                or Argument (1) = "?")));

      function To_Pixel_Value (X : Unbounded_String) return Frames.Pixel_Value
      is (Frames.Pixel_Value'Value (To_String (X)));



      Parsed_Options       : CL_Parser.Option_Values;

      procedure Parse_Options_And_Apply_Defaults (Missing_Options : out Unbounded_String)
      is
      begin
         CL_Parser.Parse (Names  => Option_Specs,
                          Result => Parsed_Options);

         CL_Parser.Apply_Defaults (Values   => Parsed_Options,
                                   Missing  => Missing_Options,
                                   Defaults => Defaults);
      end Parse_Options_And_Apply_Defaults;

      procedure Set_Levels is
      begin
         Set (Min, To_Pixel_Value (Parsed_Options (Min).Value));
         Set (Max, To_Pixel_Value (Parsed_Options (Max).Value));

         if Parsed_Options (Neutral).Missing then

            Set (Neutral, (Get (Min) + Get (Max)) / 2.0);

         else
            Set (Neutral, To_Pixel_Value (Parsed_Options (Neutral).Value));

         end if;
      end Set_Levels;


      procedure Set_Sampling_Spec (Msg : out Unbounded_String) is
         use Timestamps;

         T : Timestamps.Duration;
      begin
         T := Value (To_String (Parsed_Options (Frame_Rate).Value));


         Set (Sampling_Period, T);
         --  Set (Start_Time, Parse_Start_Time (To_String (Parsed_Options (Start_Time).Value)));
         --  Set (Stop_Time, Parse_Stop_Time (To_String (Parsed_Options (Stop_Time).Value)));

         Msg := Null_Unbounded_String;
      end Set_Sampling_Spec;

      --  procedure Set_Negative_Event_Action (Err : out Unbounded_String)
      --  is
      --  begin
      --     if Parsed_Options (On_Negative).Missing then
      --        Data.Set_Negative_Event_Action (Keep);
      --        Err := Null_Unbounded_String;
      --
      --        return;
      --     end if;
      --
      --
      --     declare
      --        Action : constant String := To_String (Parsed_Options (On_Negative).Value);
      --     begin
      --        if Action = "keep" or Action = "k" then
      --           Data.Set_Negative_Event_Action (Keep);
      --           Err := Null_Unbounded_String;
      --
      --        elsif Action = "rectify" or Action = "rect" or Action = "r"  then
      --           Data.Set_Negative_Event_Action (Rectify);
      --           Err := Null_Unbounded_String;
      --
      --        elsif Action = "clip" or Action = "c" or Action = "0"
      --          or Action = "ignore"
      --        then
      --           Data.Set_Negative_Event_Action (Clip);
      --           Err := Null_Unbounded_String;
      --
      --        else
      --           Err := To_Unbounded_String ("Unknown negative event action '" & Action & "'");
      --        end if;
      --     end;
      --  end Set_Negative_Event_Action;

      procedure Set_Start_And_Stop_Times is

         Start : Timestamps.Timestamp := Timestamps.Minus_Infinity;
         Stop  : Timestamps.Timestamp := Timestamps.Infinity;
      begin
         Start := (if Parsed_Options (Start_Time).Missing then
                      Timestamps.Minus_Infinity
                   else
                      Timestamps.Value (To_String (Parsed_Options (Start_Time).Value)));

         Stop := (if Parsed_Options (Stop_Time).Missing then
                     Timestamps.Infinity
                  else
                     Timestamps.Value (To_String (Parsed_Options (Stop_Time).Value)));

         --  if not Parsed_Options (Synch_With).Missing then
         --     declare
         --        Filename : constant String :=
         --                     To_String (Parsed_Options (Synch_With).Value);
         --
         --        Events   : Event_Sequences.Event_Sequence;
         --        Metadata : Event_Sequences.Metadata_Map;
         --     begin
         --        Event_Streams.Read_Event_Stream (Filename               => Filename,
         --                                         Use_Absolute_Timestamp => True,
         --                                         Events                 => Events,
         --                                         Metadata               => Metadata,
         --                                         Negative_Event_Weight  => 1);
         --
         --        Start := Timestamps.Max (Start, Event_Sequences.T_Min (Events));
         --        Stop := Timestamps.Min (Stop, Event_Sequences.T_Max (Events));
         --     end;
         --  end if;

         Set (Start_Time, Start);
         Set (Stop_Time, Stop);
      end Set_Start_And_Stop_Times;

      --  procedure Handle_Metadata_Request is
      --  begin
      --     if Parsed_Options (Metadata_Filename).Missing then
      --        Set (Metadata_Filename, "");
      --
      --     elsif Parsed_Options (Metadata_Filename).Value /= Null_Unbounded_String then
      --        Set (Metadata_Filename, To_String (Parsed_Options (Metadata_Filename).Value));
      --
      --     else
      --        Set (Metadata_Filename, To_String (Output_Filename_Template.Head) & ".meta");
      --
      --     end if;
      --  end Handle_Metadata_Request;

      procedure Handle_First_Image (Err : out Unbounded_String)  is
         First_Image_Value : constant String :=
                               To_String (Parsed_Options (First_Image).Value);
      begin
         if First_Image_Value = "" then
            Set_First_Image_Spec ((Class    => Uniform,
                                   Level    => Get (Neutral)));

         elsif Patterns.Is_Float (First_Image_Value) then
            declare
               use Frames;

               Level : constant Pixel_Value :=
                         Pixel_Value'Value (First_Image_Value);
            begin
               if Level > Get (Max) or Level < Get (Min) then
                  Err := To_Unbounded_String ("First image level out of bounds");
                  return;
               end if;

               Set_First_Image_Spec ((Class    => Uniform,
                                      Level    => Level));

            end;

         else
            pragma Assert (First_Image_Value /= "");

            Set_First_Image_Spec ((Class    => External,
                                   Filename => To_Unbounded_String (First_Image_Value)));

         end if;

         Err := Null_Unbounded_String;

      end Handle_First_Image;

      function Make_Report (Msg : Unbounded_String) return Parsing_Report
      is (Parsing_Report'(Status  => Bad_Command_Line,
                          Message => Msg));

      Error : Unbounded_String;
   begin
      if Help_Asked then
         Report := Parsing_Report'(Status  => Full_Help_Asked,
                                   Message => Null_Unbounded_String);

         return;
      end if;


      Set_Verbosity_Level ((if Is_A_Tty (2) then Interactive else Logging));

      declare
         Missing_Options : Unbounded_String;
      begin
         Parse_Options_And_Apply_Defaults (Missing_Options);

         if Missing_Options /= Null_Unbounded_String then
            Report := Parsing_Report'(Status  => Bad_Command_Line,
                                      Message =>  "Missing mandatory options: " & Missing_Options);

            return;
         end if;
      end;

      Set_Levels;

      Set_Start_And_Stop_Times;

      Set_Sampling_Spec (Error);

      if Error /= Null_Unbounded_String then
         Report := Make_Report (Error);
         return;
      end if;

      Handle_First_Image (Error);

      if Error /= Null_Unbounded_String then
         Report := Make_Report (Error);
         return;
      end if;

      Set (Data.Output_Filename_Template,
           To_String (Parsed_Options (Output_Template).Value));


      Set (Data.Log_Progress, To_String (Parsed_Options (Log_Progress).Value));


      if Parsed_Options (Input_Spec).Missing  then
         Set (Input_Filename, "-");
      else
         Set (Input_Filename, To_String (Parsed_Options (Input_Spec).Value));
      end if;

      Set (Event_Weigth, To_Pixel_Value (Parsed_Options (Event_Weigth).Value));

      --  Set_Negative_Event_Action (Error);

      if Error /= Null_Unbounded_String then
         Report := Make_Report (Error);
         return;
      end if;

      pragma Assert (Is_All_Set);

      Report := Parsing_Report'(Status  => Success,
                                Message => Null_Unbounded_String);
   end Parse_Command_Line;


   ------------------------------
   -- Number_Of_Parallel_Tasks --
   ------------------------------

   function Number_Of_Parallel_Tasks return System.Multiprocessors.CPU
   is (if Is_Set (N_Tasks) then
          System.Multiprocessors.CPU (Integer'(Get (N_Tasks)))
       else
          System.Multiprocessors.Number_Of_CPUs);

   -----------------------------
   -- Frame_Filename_Template --
   -----------------------------

   function Frame_Filename_Template  return String
   is (Data.Get (Data.Output_Filename_Template));

   --------------
   -- N_Inputs --
   --------------

   function N_Inputs return Positive
   is (Data.N_Inputs);

   --------------------
   -- Input_Filename --
   --------------------

   function Input_Filename (N : Positive) return String
   is (Data.Get_Input_Filename (N));

   ---------------------
   -- Sampling_Period --
   ---------------------

   function Frame_Period return Timestamps.Duration
   is (Get (Sampling_Period));

   ------------------
   -- Oversampling --
   ------------------

   function Oversampling return Positive
   is (Get (Data.Oversampling));

   --------------
   -- Start_At --
   --------------

   function Start_At  return Timestamps.Timestamp
   is (Get (Start_Time));


   -------------
   -- Stop_At --
   -------------

   function Stop_At return Timestamps.Timestamp
   is (Get (Stop_Time));



   --------------------------
   -- Start_Image_Filename --
   --------------------------

   function Start_Image_Spec return Start_Image_Spec_Type
   is (Get_First_Image_Spec);


   function Initial_Image (Size_X : Positive;
                           Size_Y : Positive)
                           return Frames.Image_Type
   is
      use Frames;
   begin
      case Start_Image_Spec.Class is
         when Uniform =>
            return Frames.Uniform (Size_X, Size_Y, Start_Image_Spec.Level);

         when External =>
            return Result : constant Image_Type :=
              Frames.Load (To_String (Start_Image_Spec.Filename)) do

               if Result'Length (1) /= Size_X or Result'Length (2) /= Size_Y then
                  raise Constraint_Error
                    with "Non-compatible start image size";
               end if;
            end return;
      end case;
   end Initial_Image;


   function Verbosity_Level return Verbosity
   is (Config.Data.Verbosity_Level);


   function Short_Help_Text return String
   is ("Usage: "
       & Sparked_Command_Line.Command_Name
       & " memory-spec  sampling  radix  [event-filename] [first-image]");

   function Long_Help_Text return String
   is
      use Ada.Characters.Latin_9;
   begin
      return Short_Help_Text
        & LF
        & LF
        & "memory-spec: " & LF
        & "   step (or s)" & LF
        & "   linear:t (or lin:t or l:t)" & LF
        & "   exponential:t (or exp:t or e:t)" & LF
        & LF
        & "sampling: sampling period (framerate). Examples" & LF
        & "  in timestamp units    : 1234 (no unit specified)" & LF
        & "  in seconds, ms, etc   : 1s   1ms  1000ns" & LF
        & "  in frame per seconds  : 1000fps" & LF
        & LF
        & "Note: NO SPACE between number and unit" & LF
        & LF
        & "radix: used to generate the filenames of the frames." & LF
        & "'" & Frame_Number_Marker & "' is replaced by the frame number" & LF
        & "(" & Strip_Spaces (Frame_Number_Default_Width'Image) & " digits"
        & " left padded with '" & Frame_Number_Padding_Char & "')" & LF
        & LF
        & "event-filename: optional, the file with the camera events." & LF
        & "If missing the standard input is read"
        & LF;
   end Long_Help_Text;

   function Event_Contribution return Frames.Pixel_Value
   is (Get (Event_Weigth));

   function Pixel_Min return Frames.Pixel_Value
   is (Get (Min));

   function Pixel_Max return Frames.Pixel_Value
   is (Get (Max));


   function Log_Progress return Boolean
   is (Get (Log_Progress) /= "");

   function Log_Progress_Filename return String
   is (Get (Log_Progress));



   procedure Dump_Cli is
      use Ada.Command_Line;
   begin
      Put_Line (Standard_Error, "CLI DUMP BEGIN");

      for I in 1 .. Argument_Count loop
         Put_Line (Standard_Error, "[" & Argument (I) & "]");
      end loop;

      Put_Line (Standard_Error, "CLI DUMP END");
      New_Line (Standard_Error);
   end Dump_Cli;

end DVAccum.Config;
