pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Text_IO; use Ada.Text_IO;

with Aida.Command_Line;
with Ada.Strings.Fixed;        use Ada.Strings;


with Ada.Characters.Latin_9;
with Generic_Command_Line_Parser;

with Interfaces.C;

use Interfaces;

with Config.Syntax;  use Config.Syntax;
with Config.Data;    use Config.Data;

with Patterns;
with Event_Sequences;
with Event_Streams;
with Ada.Command_Line;


package body Config with SPARK_Mode is
   use type Times.Timestamp;

   T0_Has_Been_Fixed : Boolean := False;

   function T0_Fixed return Boolean
   is (T0_Has_Been_Fixed);

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
      use Aida.Command_Line;
      use type Images.Pixel_Value;

      type Options is
        (
         Decay,
         Sampling,
         Frame_Rate,
         Output_Template,
         Input_Spec,
         Log_Progress,
         Synch_With,
         First_Image,
         Metadata_Filename,
         Start_Time,
         Stop_Time,
         Min,
         Max,
         Neutral,
         Event_Weigth,
         On_Negative,
         Lazy_Decay
        );

      package CL_Parser is
        new Generic_Command_Line_Parser (Options);

      Option_Specs : constant CL_Parser.Option_Names :=
                       (
                        Decay                => +"decay",
                        Sampling             => +"sampling",
                        Frame_Rate           => +"framerate|frame-rate|fps",
                        Output_Template      => +"output",
                        Input_Spec           => +"input",
                        Log_Progress         => +"log-progress|log-to|progress|log",
                        Synch_With           => +"synch-with|synch",
                        First_Image          => +"first-image|first",
                        Metadata_Filename    => +"metadata|meta",
                        Start_Time           => +"start",
                        Stop_Time            => + "stop",
                        Min                  => +"min",
                        Max                  => +"max",
                        Neutral              => +"neutral",
                        Event_Weigth         => +"weight",
                        On_Negative          => +"on-negative|neg",
                        Lazy_Decay           => +"lazy"
                       );

      Defaults : constant CL_Parser.Option_Defaults :=
                   (
                    Decay                => CL_Parser.Mandatory_Option,
                    Sampling             => CL_Parser.Ignore_If_Missing,
                    Frame_Rate           => CL_Parser.Ignore_If_Missing,
                    Output_Template      => CL_Parser.Mandatory_Option,
                    Input_Spec           => CL_Parser.Ignore_If_Missing,
                    Log_Progress         => (CL_Parser.Use_Default, +""),
                    Synch_With           => CL_Parser.Ignore_If_Missing,
                    First_Image          => (CL_Parser.Use_Default, +""),
                    Metadata_Filename    => CL_Parser.Ignore_If_Missing,
                    Start_Time           => CL_Parser.Ignore_If_Missing,
                    Stop_Time            => CL_Parser.Ignore_If_Missing,
                    Min                  => (CL_Parser.Use_Default, +"-1.0"),
                    Max                  => (CL_Parser.Use_Default, +"1.0"),
                    Neutral              => CL_Parser.Ignore_If_Missing,
                    Event_Weigth         => (CL_Parser.Use_Default, +"0.25"),
                    On_Negative          => CL_Parser.Ignore_If_Missing,
                    Lazy_Decay           => CL_Parser.Ignore_If_Missing
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

      procedure Set_Decay
        with
          Pre => not Is_Set (Decay),
          Post => Is_Set (Decay);


      function Help_Asked return Boolean
      is (Argument_Count = 0 or else
            (Argument_Count = 1 and then
               (Argument (1) = "help"
                or Argument (1) = "h"
                or Argument (1) = "-h"
                or Argument (1) = "--help"
                or Argument (1) = "?")));

      function To_Pixel_Value (X : Unbounded_String) return Images.Pixel_Value
      is (Images.Pixel_Value'Value (To_String (X)));



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

      procedure Set_Decay is
         use Memory_Dynamic;

         Chosen_Decay : constant Decay_Spec :=
                          Parse_Memory_Spec (To_String (Parsed_Options (Decay).Value));
      begin
         case Chosen_Decay.Class is
            when None =>
               Set_Decay (No_Decay);

            when Linear =>
               Set_Decay (Linear (T       => Chosen_Decay.Tau,
                                  Neutral => Get (Neutral)));

            when Exponential =>
               Set_Decay (Exponential (T          => Chosen_Decay.Tau,
                                       Zero_Level => Get (Neutral)));

            when Reset =>
               Set_Decay (Step (Reset_To => Get (Neutral)));

         end case;
      end Set_Decay;


      procedure Set_Sampling_Spec (Msg : out Unbounded_String) is
         use Times;

         T : Times.Duration;
      begin
         if Parsed_Options (Sampling).Missing and not Parsed_Options (Frame_Rate).Missing then
            T := Value (To_String (Parsed_Options (Frame_Rate).Value & "fps"));

         elsif not Parsed_Options (Sampling).Missing and Parsed_Options (Frame_Rate).Missing then
            T := Value (To_String (Parsed_Options (Sampling).Value));

         else
            Msg := To_Unbounded_String ("Sampling or framerate, but not both");
            return;
         end if;


         Set (Sampling_Period, T);
         --  Set (Start_Time, Parse_Start_Time (To_String (Parsed_Options (Start_Time).Value)));
         --  Set (Stop_Time, Parse_Stop_Time (To_String (Parsed_Options (Stop_Time).Value)));

         Msg := Null_Unbounded_String;
      end Set_Sampling_Spec;

      procedure Set_Negative_Event_Action (Err : out Unbounded_String)
      is
      begin
         if Parsed_Options (On_Negative).Missing then
            Data.Set_Negative_Event_Action (Keep);
            Err := Null_Unbounded_String;

            return;
         end if;


         declare
            Action : constant String := To_String (Parsed_Options (On_Negative).Value);
         begin
            if Action = "keep" or Action = "k" then
               Data.Set_Negative_Event_Action (Keep);
               Err := Null_Unbounded_String;

            elsif Action = "rectify" or Action = "rect" or Action = "r"  then
               Data.Set_Negative_Event_Action (Rectify);
               Err := Null_Unbounded_String;

            elsif Action = "clip" or Action = "c" or Action = "0"
              or Action = "ignore"
            then
               Data.Set_Negative_Event_Action (Clip);
               Err := Null_Unbounded_String;

            else
               Err := To_Unbounded_String ("Unknown negative event action '" & Action & "'");
            end if;
         end;
      end Set_Negative_Event_Action;

      procedure Set_Start_And_Stop_Times is

         Start : Times.Timestamp := Times.Minus_Infinity;
         Stop  : Times.Timestamp := Times.Infinity;
      begin
         Start := (if Parsed_Options (Start_Time).Missing then
                      Times.Minus_Infinity
                   else
                      Times.Value (To_String (Parsed_Options (Start_Time).Value)));

         Stop := (if Parsed_Options (Stop_Time).Missing then
                     Times.Infinity
                  else
                     Times.Value (To_String (Parsed_Options (Stop_Time).Value)));

         if not Parsed_Options (Synch_With).Missing then
            declare
               Filename : constant String :=
                            To_String (Parsed_Options (Synch_With).Value);

               Events   : Event_Sequences.Event_Sequence;
               Metadata : Event_Sequences.Metadata_Map;
            begin
               Event_Streams.Read_Event_Stream (Filename               => Filename,
                                                Use_Absolute_Timestamp => True,
                                                Events                 => Events,
                                                Metadata               => Metadata,
                                                Negative_Event_Weight  => 1);

               Start := Times.Max (Start, Event_Sequences.T_Min (Events));
               Stop := Times.Min (Stop, Event_Sequences.T_Max (Events));
            end;
         end if;

         Set (Start_Time, Start);
         Set (Stop_Time, Stop);
      end Set_Start_And_Stop_Times;

      procedure Handle_Metadata_Request is
      begin
         if Parsed_Options (Metadata_Filename).Missing then
            Set (Metadata_Filename, "");

         elsif Parsed_Options (Metadata_Filename).Value /= Null_Unbounded_String then
            Set (Metadata_Filename, To_String (Parsed_Options (Metadata_Filename).Value));

         else
            Set (Metadata_Filename, To_String (Output_Filename_Template.Head) & ".meta");

         end if;
      end Handle_Metadata_Request;

      procedure Handle_First_Image (Err : out Unbounded_String)  is
         First_Image_Value : constant String :=
                               To_String (Parsed_Options (First_Image).Value);
      begin
         if First_Image_Value = "" then
            Set_First_Image_Spec ((Class    => Uniform,
                                   Level    => Get (Neutral)));

         elsif Patterns.Is_Float (First_Image_Value) then
            declare
               use Images;

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

      Set_Decay;

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

      Set_Output_Filename_Template
        (Parse_Output_Filename_Template (Parsed_Options (Output_Template).Value));

      Set (Data.Log_Progress, To_String (Parsed_Options (Log_Progress).Value));

      Handle_Metadata_Request;

      if Parsed_Options (Input_Spec).Missing  then
         Set (Input, "-");
      else
         Set (Input, To_String (Parsed_Options (Input_Spec).Value));
      end if;

      Set (Event_Weigth, To_Pixel_Value (Parsed_Options (Event_Weigth).Value));

      Set (Lazy_Decay, not Parsed_Options (Lazy_Decay).Missing);

      Set_Negative_Event_Action (Error);

      if Error /= Null_Unbounded_String then
         Report := Make_Report (Error);
         return;
      end if;

      pragma Assert (Is_All_Set);

      Report := Parsing_Report'(Status  => Success,
                                Message => Null_Unbounded_String);
   end Parse_Command_Line;

   ------------
   -- Fix_T0 --
   ------------

   procedure Fix_T0 (T0 : Times.Timestamp) is
   begin
      for Field in Data.Timestamp_Field loop
         if Times.Is_Relative (Get (Field)) then
            Update (Field, Times.Fix_T0 (T => Get (Field), T0 => T0));
         end if;
      end loop;

      T0_Has_Been_Fixed := True;
   end Fix_T0;

   -----------
   -- Input --
   -----------

   function Input return String
   is (Get (Input));

   ---------------------
   -- Sampling_Period --
   ---------------------

   function Sampling_Period return Times.Duration
   is (Get (Sampling_Period));

   --------------
   -- Start_At --
   --------------

   function Start_At  return Times.Timestamp
   is (Get (Start_Time));


   -------------
   -- Stop_At --
   -------------

   function Stop_At return Times.Timestamp
   is (Get (Stop_Time));


   -----------------------
   -- Forgetting_Method --
   -----------------------

   function Forgetting_Method return Memory_Dynamic.Dynamic_Type
   is (Config.Data.Decay);

   -------------------
   -- Output_Format --
   -------------------

   function Output_Format return Images.Format_Type
   is (Config.Data.Output_Filename_Template.Frame_Format);

   -----------
   -- Radix --
   -----------


   function Frame_Filename (N : Frame_Index) return String
   is

      Frame_Filename_Spec : constant Radix_Spec :=
                              Config.Data.Output_Filename_Template;

      function Format_Frame_Number (N : Frame_Index) return String
      is
         use Ada.Strings.Fixed;

         Raw_Image : constant String := Strip_Spaces (Frame_Index'Image (N));

         Padding_Length : constant Integer :=
                            Frame_Filename_Spec.Frame_Number_Width - Raw_Image'Length;

         Padding   : constant String :=
                       (if Padding_Length <= 0
                        then
                           ""
                        else
                           Padding_Length * Frame_Filename_Spec.Padding_Char);
      begin
         return Padding & Raw_Image;
      end Format_Frame_Number;

   begin
      return To_String (Frame_Filename_Spec.Head &
                          Format_Frame_Number (N) &
                          Frame_Filename_Spec.Tail);
   end Frame_Filename;


   --------------------------
   -- Start_Image_Filename --
   --------------------------

   function Start_Image_Spec return Start_Image_Spec_Type
   is (Get_First_Image_Spec);


   function Start_Image (Size_X : Camera_Events.X_Coordinate_Type;
                         Size_Y : Camera_Events.Y_Coordinate_Type)
                         return Images.Image_Type
   is
      use Camera_Events;
   begin
      case Start_Image_Spec.Class is
         when Uniform =>
            return Images.Uniform (Size_X, Size_Y, Start_Image_Spec.Level);

         when External =>
            return Result : constant Images.Image_Type :=
              Images.Load (To_String (Start_Image_Spec.Filename)) do

               if Result'Length (1) /= Size_X or Result'Length (2) /= Size_Y then
                  raise Constraint_Error
                    with "Non-compatible start image size";
               end if;
            end return;
      end case;
   end Start_Image;


   function Verbosity_Level return Verbosity
   is (Config.Data.Verbosity_Level);


   function Short_Help_Text return String
   is ("Usage: "
       & Aida.Command_Line.Command_Name
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

   function Event_Contribution return Images.Pixel_Value
   is (Get (Event_Weigth));

   function Pixel_Min return Images.Pixel_Value
   is (Get (Min));

   function Pixel_Max return Images.Pixel_Value
   is (Get (Max));

   function Synchronous_Update return Boolean
   is (Get (Lazy_Decay));

   function Reset_Each_Frame return Boolean
   is (Memory_Dynamic.Is_Reset (Config.Data.Decay));

   function Neutral_Value return Images.Pixel_Value
   is (Get (Neutral));

   function On_Negative_Event return Negative_Event_Action
   is (Get_Negative_Event_Action);

   function Negative_Weight return Integer
   is
     (case On_Negative_Event is
         when Keep => 1,
         when Rectify => -1,
         when Clip => 0
     );



   --  function Rectify_Events return Boolean
   --  is (Get (Rectify));

   function Metadata_Requested return Boolean
   is (Get (Metadata_Filename) /= "");

   function Metadata_Filename return String
   is (Get (Metadata_Filename));

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

end Config;
