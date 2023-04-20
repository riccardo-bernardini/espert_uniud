with Camera_Events;
with Memory_Dynamic;
with Images;

package Config is
   function Package_Ready return Boolean;

   procedure Parse_Command_Line
     with
       Pre => not Package_Ready,
       Post => Package_Ready;

   Bad_Command_Line : exception;


   Full_Help_Asked : exception;

   function Short_Help_Text return String;

   function Long_Help_Text return String;

   type Frame_Index is range 0 .. Integer'Last;

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

   function Verbose return Boolean
     with
       Pre => Package_Ready;

   function Show_Progress_Bar return Boolean
     with
       Pre => Package_Ready;

end Config;
