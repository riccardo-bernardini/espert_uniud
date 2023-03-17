with Ada.Text_IO;
with Camera_Events;
with Memory_Dynamic;

package Config is
   procedure Parse_Command_Line;

   function Input return Ada.Text_IO.File_Access;

   function Sampling_Period return Camera_Events.Duration;

   function Forgetting_Method return  Memory_Dynamic.Dynamic_Type;

   function Has_Start_Image return Boolean;

   function Start_Image_Filename return String
     with
       Pre => Has_Start_Image;
end Config;
