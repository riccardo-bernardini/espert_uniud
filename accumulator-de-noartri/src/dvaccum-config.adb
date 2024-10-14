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

      use CL_Parser;

      Option_Specs : constant CL_Parser.CLI_Syntax :=
                       (
                        Filter               => Option ("filter") and Mandatory,
                        Oversampling         => Option ("oversampling|over [1]"),
                        Parallel             => Option ("n-tasks|parallel"),
                        Frame_Rate           => Option ("framerate|frame-rate|fps") and Mandatory,
                        Output_Template      => Option ("output [%b-%d.png]"),
                        Log_Progress         => Option ("log-progress|log-to|progress|log []"),
                        First_Image          => Option ("first-image|first [neutral]"),
                        Start_Time           => Option ("start [0]"),
                        Stop_Time            => Option ("stop [inf]"),
                        Min                  => Option ("min [0.0]"),
                        Max                  => Option ("max [255.0]"),
                        Neutral              => Option ("neutral [0.0]"),
                        Event_Weigth         => Option ("gain|weight [1.0]")
                       );




      procedure Set_Levels (Min, Max, Neutral : Float)
        with
          Pre => not Is_Set (Data.Min)
          and not Is_Set (Data.Max)
          and not Is_Set (Data.Neutral),
          Post => Is_Set (Data.Min)
          and Is_Set (Data.Max)
          and Is_Set (Data.Neutral);

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


      procedure Set_Levels (Min, Max, Neutral : Float) is
         use Frames;
      begin
         Set (Data.Min, Pixel_Value (Min));
         Set (Data.Max, Pixel_Value (Max));
         Set (Data.Neutral, Pixel_Value (Neutral));
      end Set_Levels;


      function Parse_First_Image_Spec (Spec : String)
                                       return Start_Image_Spec_Type
        with
          Pre => Data.Is_Set (Neutral)
          and Data.Is_Set (Max)
          and Data.Is_Set (Min);

      function Parse_First_Image_Spec (Spec : String)
                                       return Start_Image_Spec_Type
      is
      begin
         if Spec = "" or Spec = "neutral" then
            return Start_Image_Spec_Type'(Class    => Uniform,
                                          Level    => Get (Neutral));

         elsif Patterns.Is_Float (Spec) then
            declare
               use Frames;

               Level : constant Pixel_Value :=
                         Pixel_Value'Value (Spec);
            begin
               if Level > Get (Max) or Level < Get (Min) then
                  raise Bad_Syntax
                    with "Initial level out of bounds";
               end if;

               return Start_Image_Spec_Type'((Class    => Uniform,
                                              Level    => Level));

            end;

         else
            pragma Assert (Spec /= "");

            return Start_Image_Spec_Type'(Class    => External,
                                          Filename => To_Unbounded_String (Spec));

         end if;

      end Parse_First_Image_Spec;

      function Missing_Options_Report (Opts : Option_Lists.List)
                                       return Parsing_Report
      is
         function Join (Opts : Option_Lists.List) return Unbounded_String
         is
            Result : Unbounded_String;
         begin
            for O of Opts loop
               Result := Result & Options'Image (O);
            end loop;

            return Result;
         end Join;

      begin
         return Parsing_Report'(Status  => Bad_Command_Line,
                                Message =>
                                  "Missing mandatory options: " & Join (Opts));

      end Missing_Options_Report;
   begin
      if Help_Asked then
         Report := Parsing_Report'(Status  => Full_Help_Asked,
                                   Message => Null_Unbounded_String);

         return;
      end if;


      Set_Verbosity_Level ((if Is_A_Tty (2) then
                              Interactive
                           else
                              Logging));

      declare
         use Timestamps;
         use Frames;

         Parsed_Options : constant Parsed_CL := Parse_CL (Option_Specs);
      begin
         if not Parsed_Options.Missing_Options.Is_Empty then
            Report := Missing_Options_Report (Parsed_Options.Missing_Options);

            return;
         end if;

         Set_Levels (Min     => Parsed_Options (Min),
                     Max     => Parsed_Options (Max),
                     Neutral => Parsed_Options (Neutral));

         Set (Data.Start_Time, Timestamp'(Value (Parsed_Options (Start_Time))));

         Set (Data.Stop_Time, Timestamp'(Value (Parsed_Options (Stop_Time))));

         Set (Sampling_Period, Timestamps.Duration'(Value (Parsed_Options (Frame_Rate))));

         Set_First_Image_Spec (Parse_First_Image_Spec (Parsed_Options (First_Image)));

         Set (Data.Output_Filename_Template, String'(Parsed_Options (Output_Template)));

         Set (Data.Log_Progress, String'(Parsed_Options (Log_Progress)));

         Set (Data.Event_Weigth, Pixel_Value (Float'(Parsed_Options (Event_Weigth))));

         Set (Data.Filter_Spec, String'(Parsed_Options (Filter)));

         Set (Data.Oversampling, Integer'(Parsed_Options (Oversampling)));

         if not Parsed_Options.Is_Defined (Parallel) then
            Set (Data.N_Tasks, Integer (System.Multiprocessors.Number_Of_CPUs));
         else
            Set (Data.N_Tasks, Integer'(Parsed_Options (Parallel)));
         end if;


         if Parsed_Options.Argument_Count = 0 then
            Add_Input_Filename ("-");

         else
            for I in 1 .. Parsed_Options.Argument_Count loop
               Add_Input_Filename (Parsed_Options.Argument (I));
            end loop;
         end if;
      end;

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
