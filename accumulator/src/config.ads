with Ada.Text_IO;
with Camera_Events;
with Memory_Dynamic;

package Config is
   function Package_Ready return Boolean;

   procedure Parse_Command_Line
     with
       Pre => not Package_Ready,
       Post => Package_Ready;

   Bad_Command_Line : exception;

   type Frame_Index is range 0 .. Integer'Last;

   function Frame_Filename (N : Frame_Index) return String
     with
       Pre => Package_Ready;

   type File_Access is access all Ada.Text_IO.File_Type;

   function Input return File_Access
     with
       Pre => Package_Ready;

   function Sampling_Period return Camera_Events.Duration
     with
       Pre => Package_Ready;

   function Forgetting_Method return  Memory_Dynamic.Dynamic_Type
     with
       Pre => Package_Ready;


   function Has_Start_Image return Boolean
     with
       Pre => Package_Ready;

   function Start_Image_Filename return String
     with
       Pre => Package_Ready and then Has_Start_Image;
end Config;
