pragma Ada_2012;

with Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings;

with Ada.Characters.Handling;

package body Config is
   type Radix_Spec is
      record
         Head : Unbounded_String;
         Tail : Unbounded_String;
      end record;

   I_Am_Ready : Boolean := False;

   Memory_Dynamic_Spec : Memory_Dynamic.Dynamic_Type;

   type File_Pt is access all Ada.Text_IO.File_Type;

   Input_Stream : File_Pt := null;

   Sampling_Step : Camera_Events.Duration;

   Frame_Filename_Spec : Radix_Spec;

   function Package_Ready return Boolean
   is (I_Am_Ready);

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => s,
                   Side   => Both));

   procedure Parse_Command_Line is
      use type Ada.Text_Io.File_Access;
      use Ada.Command_Line;

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
               raise Bad_Command_Line with "'linear' needs a parameter";
            end if;

            return Memory_Dynamic.Linear (Camera_Events.Value (Parameter));

         elsif Method = "exponential" or Method = "exp" or Method = "e" then
            if Parameter = "" then
               raise Bad_Command_Line with "'exponential' needs a parameter";
            end if;

            return Memory_Dynamic.Exponential (Camera_Events.Value (Parameter));

         else
            raise Bad_Command_Line with "Unknown dynamic '" & method & "'";
         end if;
      end Parse_Memory_Spec;
         begin
            --
            -- CL syntax
            --
            --    main memory-spec  sampling-step  radix  [inpur-filename]
            --
            if Argument_Count < 3 or Argument_Count > 4 then
               raise Bad_Command_Line with "Wrong argument count";
            end if;


            Memory_Dynamic_Spec := Parse_Memory_Spec (Argument (1));

            Sampling_Step := Parse_Sampling_Spec (Argument (2));

            Frame_Filename_Spec := Parse_Radix (Argument (3));

            if Argument_Count = 3 then
               Input_Stream := new Ada.Text_IO.File_Type'(Ada.Text_IO.Standard_Input);

            else
               Input_Stream := new Ada.Text_IO.File_Type;

               Ada.Text_IO.Open (File => Input_Stream.all,
                                 Mode => Ada.Text_IO.In_File,
                                 Name => Argument (4));
            end if;


            pragma Assert (Input_Stream /= null
                           and then
                           Ada.Text_Io.Is_Open (Input_Stream.all));

            I_Am_Ready := True;

         end Parse_Command_Line;

         -----------
         -- Input --
         -----------

         function Input return Ada.Text_IO.File_Access is
         begin
            pragma Compile_Time_Warning (Standard.True, "Input unimplemented");
            return raise Program_Error with "Unimplemented function Input";
         end Input;

         ---------------------
         -- Sampling_Period --
         ---------------------

         function Sampling_Period return Camera_Events.Duration is
         begin
            pragma Compile_Time_Warning
              (Standard.True, "Sampling_Period unimplemented");
            return raise Program_Error with "Unimplemented function Sampling_Period";
         end Sampling_Period;

         -----------------------
         -- Forgetting_Method --
         -----------------------

         function Forgetting_Method return Memory_Dynamic.Dynamic_Type is
         begin
            pragma Compile_Time_Warning
              (Standard.True, "Forgetting_Method unimplemented");
            return
            raise Program_Error with "Unimplemented function Forgetting_Method";
         end Forgetting_Method;

         -----------
         -- Radix --
         -----------


         function Frame_Filename (N : Frame_Index) return String
         is
         begin
            pragma Compile_Time_Warning (Standard.True, "Radix unimplemented");
            return raise Program_Error with "Unimplemented function Radix";
         end Frame_Filename;

         ---------------------
         -- Has_Start_Image --
         ---------------------

         function Has_Start_Image return Boolean is
         begin
            pragma Compile_Time_Warning
              (Standard.True, "Has_Start_Image unimplemented");
            return raise Program_Error with "Unimplemented function Has_Start_Image";
         end Has_Start_Image;

         --------------------------
         -- Start_Image_Filename --
         --------------------------

         function Start_Image_Filename return String is
         begin
            pragma Compile_Time_Warning
              (Standard.True, "Start_Image_Filename unimplemented");
            return
            raise Program_Error with "Unimplemented function Start_Image_Filename";
         end Start_Image_Filename;

      end Config;
