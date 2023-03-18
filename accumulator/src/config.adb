pragma Ada_2012;

with Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings;

with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;

package body Config is
   type Radix_Spec is
      record
         Head               : Unbounded_String;
         Tail               : Unbounded_String;
         Frame_Number_Width : Positive;
         Padding_Char       : Character;
      end record;

   I_Am_Ready : Boolean := False;

   Memory_Dynamic_Spec : Memory_Dynamic.Dynamic_Type;

   type File_Pt is access all Ada.Text_IO.File_Type;

   Input_Stream : File_Access := null;

   Sampling_Step : Camera_Events.Duration;

   Frame_Filename_Spec : Radix_Spec;

   First_Image_Filename : Unbounded_String;

   function Package_Ready return Boolean
   is (I_Am_Ready);

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));

   procedure Parse_Command_Line is
      use type Ada.Text_Io.File_Access;
      use Ada.Command_Line;

      Argument_Counter : Positive := 1;

      function Current_Argument return String
        with
          Pre => Argument_Counter <= Ada.Command_Line.Argument_Count;

      function Current_Argument return String
      is (Argument (Argument_Counter));

      procedure Next_Argument
        with
          Pre => Argument_Counter <= Ada.Command_Line.Argument_Count;

      procedure Next_Argument is
      begin
         Argument_Counter := Argument_Counter + 1;
      end Next_Argument;

      function Parse_Time_Spec (Spec : String) return Camera_Events.Duration
      is
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps.Constants;

         Stripped : constant String := Strip_Spaces (Spec);

         End_Of_Number : constant Natural :=
                           Index (Source => Stripped,
                                  Set    => Decimal_Digit_Set,
                                  Test   => Outside);

         Unit : constant String :=
                  (if End_Of_Number = 0
                   then
                      ""
                   else
                      Strip_Spaces (Stripped (End_Of_Number .. Stripped'Last)));

         Value : constant String :=
                   (if End_Of_Number = 0
                    then
                       Stripped
                    else
                       Stripped (Stripped'First .. End_Of_Number - 1));
      begin
         if Unit = "" then
            return Camera_Events.Value (Value);

         elsif Unit = "s" then
            return Camera_Events.To_Duration (Float (Integer'Value (Value)));

         elsif Unit = "ms" then
            return Camera_Events.To_Duration (1.0e-3 * Float (Integer'Value (Value)));

         elsif Unit = "us" then
            return Camera_Events.To_Duration (1.0e-6 * Float (Integer'Value (Value)));

         elsif Unit = "ns" then
            return Camera_Events.To_Duration (1.0e-9 * Float (Integer'Value (Value)));

         elsif Unit = "fps" then
            return Camera_Events.To_Duration (1.0 / Float (Integer'Value (Value)));

         else
            raise Bad_Command_Line with "Unknown unit '" & Unit & "'";
         end if;
      end Parse_Time_Spec;

      function Parse_Memory_Spec (Spec : String)
                                  return Memory_Dynamic.Dynamic_Type
      is
         use Ada.Strings.Fixed;
         use Ada.Characters.Handling;

         Colon_Pos : constant Natural := Index (Source  => Spec,
                                                Pattern => ":");

         Method : constant String :=
                    Strip_Spaces (To_Lower ((if Colon_Pos = 0
                                  then
                                     Spec
                                  else
                                     Spec (Spec'First .. Colon_Pos - 1))));

         Parameter : constant String :=
                       Strip_Spaces ((if Colon_Pos = 0
                                     then
                                        ""
                                     else
                                        Spec (Colon_Pos + 1 .. Spec'Last)));
      begin
         if Method = "step" or Method = "s" then
            if Parameter /= "" then
               raise Bad_Command_Line with "'step' with parameter";
            end if;

            return Memory_Dynamic.Step;

         elsif Method = "linear" or Method = "lin" or Method = "l" then
            if Parameter = "" then
               raise Bad_Command_Line with "'linear' needs a time constant";
            end if;

            return Memory_Dynamic.Linear (Parse_Time_Spec (Parameter));

         elsif Method = "exponential" or Method = "exp" or Method = "e" then
            if Parameter = "" then
               raise Bad_Command_Line with "'exponential' needs a time constant";
            end if;

            return Memory_Dynamic.Exponential (Parse_Time_Spec (Parameter));

         else
            raise Bad_Command_Line with "Unknown dynamic '" & Method & "'";
         end if;
      end Parse_Memory_Spec;

      function Parse_Sampling_Spec (Spec : String) return Camera_Events.Duration
      is (Parse_Time_Spec (Spec));

      function Parse_Radix (Spec : String) return Radix_Spec
      is
         use Ada.Strings.Fixed;

         function To_Unbounded (X : String) return Unbounded_String
                                renames To_Unbounded_String;

         Frame_Number_Marker : constant String := "%d";

         Frame_Number_Position : constant Natural :=
                                   Index (Source  => Spec,
                                          Pattern => Frame_Number_Marker);
      begin
         if Frame_Number_Position = 0 then
            raise Bad_Command_Line
              with "Missing '" & Frame_Number_Marker & "' in frame filename radix";
         end if;

         return Radix_Spec'
           (Head               =>
              To_Unbounded (Spec (Spec'First .. Frame_Number_Position - 1)),
            Tail               =>
              To_Unbounded (Spec (Frame_Number_Position + Frame_Number_Marker'Length .. Spec'Last)),
            Frame_Number_Width => 5,
            Padding_Char       => '0');
      end Parse_Radix;

      Input_Filename_Given : Boolean := False;
      First_Image_Given : Boolean := False;
   begin
      --
      -- CL syntax
      --
      --    main memory-spec  sampling-step  radix  [input-filename] [first-image]
      --
      if Argument_Count < 3 or Argument_Count > 5 then
         raise Bad_Command_Line with "Wrong number of arguments";

      elsif Argument_Count = 3 then
         Input_Filename_Given := False;
         First_Image_Given := False;

      elsif Argument_Count = 4 then
         Input_Filename_Given := True;
         First_Image_Given := False;

      elsif Argument_Count = 5 then
         Input_Filename_Given := True;
         First_Image_Given := True;

      else
         raise Program_Error; -- We should never arrive here
      end if;


      Memory_Dynamic_Spec := Parse_Memory_Spec (Current_Argument);
      Next_Argument;

      Sampling_Step := Parse_Sampling_Spec (Current_Argument);
      Next_Argument;

      Frame_Filename_Spec := Parse_Radix (Current_Argument);
      Next_Argument;

      if Input_Filename_Given and Current_Argument /= "-"  then
         Input_Stream := new Ada.Text_IO.File_Type;

         Ada.Text_IO.Open (File => Input_Stream.all,
                           Mode => Ada.Text_IO.In_File,
                           Name => Current_Argument);

         Next_Argument;
      else
         Input_Stream := new Ada.Text_IO.File_Type'(Ada.Text_IO.Standard_Input);

      end if;

      if First_Image_Given then
         First_Image_Filename := To_Unbounded_String (Current_Argument);
         Next_Argument;
      else
         First_Image_Filename := Null_Unbounded_String;
      end if;

      pragma Assert (Input_Stream /= null
                     and then
                     Ada.Text_Io.Is_Open (Input_Stream.all));

      I_Am_Ready := True;

   end Parse_Command_Line;

   -----------
   -- Input --
   -----------

   function Input return File_Access
   is (Input_Stream);

   ---------------------
   -- Sampling_Period --
   ---------------------

   function Sampling_Period return Camera_Events.Duration
   is (Sampling_Step);

   -----------------------
   -- Forgetting_Method --
   -----------------------

   function Forgetting_Method return Memory_Dynamic.Dynamic_Type
   is (Memory_Dynamic_Spec);

   -----------
   -- Radix --
   -----------


   function Frame_Filename (N : Frame_Index) return String
   is
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

   ---------------------
   -- Has_Start_Image --
   ---------------------

   function Has_Start_Image return Boolean
   is (First_Image_Filename /= Null_Unbounded_String);

   --------------------------
   -- Start_Image_Filename --
   --------------------------

   function Start_Image_Filename return String
   is (To_String (First_Image_Filename));


   function Start_Image return Images.Image_Type
   is
   begin
      if Has_Start_Image then
         return Images.Load (Start_Image_Filename);

      else
         return Images.Zero (Images.Default_X_Size, Images.Default_Y_Size);

      end if;
   end Start_Image;

end Config;
