with Camera_Events;
with Memory_Dynamic;
with Images;

package Config is
   type Frame_Index is range 0 .. Integer'Last;

   type Verbosity is
     (
      Quiet,        -- no verbose at all
      Logging,      -- Stuff that can go to a file
      Interactive   -- Stuff that makes sense only on a terminal (e.g., progress bar)
     );

   -- Used in contracts
   function Package_Ready return Boolean
     with Ghost;

   procedure Parse_Command_Line
     with
       Pre => not Package_Ready,
       Post => Package_Ready;


   function Short_Help_Text return String;

   function Long_Help_Text return String;


   function Frame_Filename (N : Frame_Index) return String
     with
       Pre => Package_Ready;

   function Output_Format return Images.Format_Type
     with
       Pre => Package_Ready;

   function Input return String
     with
       Pre => Package_Ready;

   function Sampling_Period return Camera_Events.Duration
     with
       Pre => Package_Ready;

   function Start_At (T_Min : Camera_Events.Timestamp)
                      return Camera_Events.Timestamp
     with
       Pre => Package_Ready;

   function Stop_At (T_Max : Camera_Events.Timestamp)
                     return Camera_Events.Timestamp
     with
       Pre => Package_Ready;


   function Forgetting_Method return  Memory_Dynamic.Dynamic_Type
     with
       Pre => Package_Ready;


   function Start_Image (Size_X : Camera_Events.X_Coordinate_Type;
                         Size_Y : Camera_Events.Y_Coordinate_Type)
                         return Images.Image_Type
     with
       Pre => Package_Ready;

   function Event_Contribution return Images.Pixel_Value
     with
       Pre => Package_Ready;

   function Pixel_Min return Images.Pixel_Value
     with
       Pre => Package_Ready;

   function Pixel_Max return Images.Pixel_Value
     with
       Pre => Package_Ready;

   function Synchronous_Update return Boolean
     with
       Pre => Package_Ready;

   function Reset_Each_Frame return Boolean
     with
       Pre => Package_Ready;

   function Reset_Value return Images.Pixel_Value
     with
       Pre => Package_Ready and then Reset_Each_Frame;


   function Rectify_Events return Boolean
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

   Bad_Command_Line : exception;


   Full_Help_Asked : exception;

end Config;
