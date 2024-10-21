package Dvaccum.Monitoring is
   type Action is (Starting, Ready, Reading, Processing, Writing);

   function Currently_Doing return Action;

   procedure Set_Number_Of_Inputs (N : Positive)
     with
       Pre => Currently_Doing = Starting,
       Post => Currently_Doing = Ready;

   function File_To_Process return Natural
     with
       Pre => Currently_Doing > Starting;

   procedure Start_Reading
     with
       Pre =>
         Currently_Doing = Ready
         and File_To_Process > 0,
         Post =>
           Currently_Doing = Reading;

   procedure Set_Number_Of_Pixels (N : Positive)
     with
       Pre => Currently_Doing = Reading,
       Post =>
         Currently_Doing = Processing
         and then Number_Of_Pixels = N;

   procedure Pixel_Read
     with
       Pre =>
         Currently_Doing = Processing
         and Number_Of_Pixels > 0,
         Post =>
           Currently_Doing = Processing
           and Number_Of_Pixels = Number_Of_Pixels'Old - 1;


   function Number_Of_Pixels return Natural
     with
       Pre => Currently_Doing = Processing;

   function Frame_To_Write return Natural
     with
       Pre => Currently_Doing = Writing;

   procedure Start_Writing (N_Frames : Positive)
     with
       Pre =>
         Currently_Doing = Processing
         and Number_Of_Pixels = 0,
         Post =>
           Currently_Doing = Writing
           and Frame_To_Write = N_Frames;

   procedure Frame_Written
     with
       Pre =>
         Currently_Doing = Writing
         and Frame_To_Write > 0,
         Post =>
           Currently_Doing = Writing
           and Frame_To_Write = Frame_To_Write'Old - 1;

   procedure Done_Writing
     with
       Pre =>
         Currently_Doing = Writing
         and Frame_To_Write = 0
         and File_To_Process > 0,
         Post =>
           Currently_Doing = Ready
           and File_To_Process = File_To_Process'Old - 1;

end Dvaccum.Monitoring;
