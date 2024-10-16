with DVAccum.Frames;
with DVAccum.Timestamps;

with System.Multiprocessors;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Dvaccum.Config with SPARK_Mode is
   -- Used in contracts
   function Package_Ready return Boolean
     with Ghost;

   --  function T0_Fixed return Boolean
   --    with Ghost;

   procedure Dump_Cli;

   type Parsing_Status is (Success, Full_Help_Asked, Bad_Command_Line);

   type Parsing_Report is
      record
         Status  : Parsing_Status;
         Message : Unbounded_String;
      end record;

   procedure Parse_Command_Line (Report : out Parsing_Report)
     with
       Pre => not Package_Ready,
       Post => (Report.Status = Success) = Package_Ready;


   type Negative_Event_Action is
     (
      Keep,
      Rectify,
      Clip
     );

   type Verbosity is
     (
      Quiet,        -- no verbose at all
      Logging,      -- Stuff that can go to a file
      Interactive   -- Stuff that makes sense only on a terminal (e.g., progress bar)
     );

   function Short_Help_Text return String;

   function Long_Help_Text return String;


   function Frame_Filename_Template  return String
     with
       Pre => Package_Ready;

   function N_Inputs return Positive
     with
       Pre => Package_Ready;

   function Input_Filename (N : Positive) return String
     with
       Pre => Package_Ready and then N <= N_Inputs;

   function Frame_Period return Timestamps.Duration
     with
       Pre => Package_Ready;

   function Oversampling return Positive
     with
       Pre => Package_Ready;

   function Start_At  return Timestamps.Timestamp
     with
       Pre => Package_Ready;

   function Stop_At  return Timestamps.Timestamp
     with
       Pre => Package_Ready;


   function Initial_Image (Size_X : Positive;
                           Size_Y : Positive)
                           return Frames.Image_Type
     with
       Pre => Package_Ready;

   function Event_Contribution return Sample_Value
     with
       Pre => Package_Ready;

   function Pixel_Min return Sample_Value
     with
       Pre => Package_Ready;

   function Pixel_Max return Sample_Value
     with
       Pre => Package_Ready;

   function Verbosity_Level return Verbosity
     with
       Pre => Package_Ready;

   function Verbose return Boolean
   is (Verbosity_Level > Quiet)
     with
       Pre => Package_Ready;

   function Show_Progress_Bar return Boolean
   is (Verbosity_Level >= Interactive)
     with
       Pre => Package_Ready;

   --  function Metadata_Requested return Boolean
   --    with
   --      Pre => Package_Ready;
   --
   --  function Metadata_Filename return String
   --    with
   --      Pre => Package_Ready and then Metadata_Requested;

   --  procedure Fix_T0 (T0 : Timestamps.Timestamp)
   --    with
   --      Pre => Package_Ready and not T0_Fixed,
   --    Post => T0_Fixed;

   function Log_Progress return Boolean
     with
       Pre => Package_Ready;

   function Log_Progress_Filename return String
     with
       Pre => Package_Ready and then Log_Progress;

   function Number_Of_Parallel_Tasks return System.Multiprocessors.CPU;

end Dvaccum.Config;
