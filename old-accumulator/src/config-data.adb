pragma Ada_2012;

package body Config.Data with SPARK_Mode is

   Set_Fields : array (Configuration_Field) of Boolean := (others => False);

   type String_Field_Array is array (String_Field) of Unbounded_String;

   type Numeric_Field_Array is array (Numeric_Field) of Images.Pixel_Value;

   type Boolean_Field_Array is array (Boolean_Field) of Boolean;

   type Duration_Field_Array is array (Duration_Field) of Times.Duration;

   type Timestamp_Field_Array is array (Timestamp_Field) of Times.Timestamp;

   --
   --  Why using a record? Wouldn't be it simpler to use just variables?
   --  No special reason, really, just the desire of keeping all the
   --  configuration data together.  Also, with this approach the record
   --  acts as a namespace, simplifying the management of names.
   --
   type Config_Data_Record is
      record
         Decay_Handler           : Memory_Dynamic.Dynamic_Type;
         Requested_Verbosity     : Verbosity;
         Input_Filename          : Unbounded_String := Null_Unbounded_String;
         Frame_Filename_Template : Config.Syntax.Radix_Spec;

         First_Image_Spec        : Start_Image_Spec_Type;

         String_Fields           : String_Field_Array;
         Numeric_Fields          : Numeric_Field_Array;
         Timestamp_Fields        : Timestamp_Field_Array;
         Duration_Fields         : Duration_Field_Array;
         Boolean_Fields          : Boolean_Field_Array;
         On_Negative_Event       : Negative_Event_Action;
      end record;

   Config_Data : Config_Data_Record;

   procedure Is_Set (Field : Configuration_Field)
     with
       Post => Is_Set (Field);

   procedure Is_Set (Field : Configuration_Field)
   is
   begin
      Set_Fields (Field) := True;
   end Is_Set;

   procedure Set_Negative_Event_Action (Action : Negative_Event_Action)
   is
   begin
      Config_Data.On_Negative_Event := Action;
      Is_Set (Negative_Event_Handling);
   end Set_Negative_Event_Action;

   function Get_Negative_Event_Action return Negative_Event_Action
   is (Config_Data.On_Negative_Event);

   procedure Set_First_Image_Spec (Spec : Start_Image_Spec_Type)
   is
   begin
      Config_Data.First_Image_Spec := Spec;
      Is_Set (First_Image);
   end Set_First_Image_Spec;

   function Get_First_Image_Spec return Start_Image_Spec_Type
   is (Config_Data.First_Image_Spec);

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Field : Configuration_Field) return Boolean
   is (Set_Fields (Field));


   --  ------------------------
   --  -- Set_Input_Filename --
   --  ------------------------
   --
   --  procedure Set_Input_Filename (Item : String) is
   --  begin
   --     Config_Data.Input_Filename := To_Unbounded_String (Item);
   --     Is_Set (Input);
   --  end Set_Input_Filename;
   --
   --  --------------------
   --  -- Input_Filename --
   --  --------------------
   --
   --  function Input_Filename return String
   --  is (To_String (Config_Data.Input_Filename));

   -------------------------
   -- Set_Verbosity_Level --
   -------------------------

   procedure Set_Verbosity_Level (Verb : Verbosity)
   is
   begin
      Config_Data.Requested_Verbosity := Verb;
      Is_Set (Verbosity_Level);
   end Set_Verbosity_Level;

   ---------------------
   -- Verbosity_Level --
   ---------------------

   function Verbosity_Level return Verbosity
   is (Config_Data.Requested_Verbosity);

   ----------------------------------
   -- Set_Output_Filename_Template --
   ----------------------------------

   procedure Set_Output_Filename_Template (Item : Syntax.Radix_Spec) is
   begin
      Config_Data.Frame_Filename_Template := Item;
      Is_Set (Output_Filename_Template);
   end Set_Output_Filename_Template;

   ------------------------------
   -- Output_Filename_Template --
   ------------------------------

   function Output_Filename_Template return Syntax.Radix_Spec
   is (Config_Data.Frame_Filename_Template);


   ---------------
   -- Set_Decay --
   ---------------

   procedure Set_Decay (Item : Memory_Dynamic.Dynamic_Type) is
   begin
      Config_Data.Decay_Handler := Item;
      Is_Set (Decay);
   end Set_Decay;

   -----------
   -- Decay --
   -----------

   function Decay return Memory_Dynamic.Dynamic_Type
   is (Config_Data.Decay_Handler);

   procedure Set (Field : String_Field;
                  Value : String) is
   begin
      Config_Data.String_Fields (Field) := To_Unbounded_String (Value);
      Is_Set (Field);
   end Set;

   function Get (Field : String_Field) return String
   is (To_String (Config_Data.String_Fields (Field)));


   ---------
   -- Set --
   ---------

   procedure Set (Field : Numeric_Field; Value : Images.Pixel_Value) is
   begin
      Config_Data.Numeric_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ------------
   -- Update --
   ------------

   procedure Update  (Field : Timestamp_Field;
                      Value : Times.Timestamp)is
   begin
      Config_Data.Timestamp_Fields (Field) := Value;
   end Update;


   ---------
   -- Get --
   ---------

   function Get (Field : Numeric_Field) return Images.Pixel_Value
   is (Config_Data.Numeric_Fields (Field));

   ---------
   -- Set --
   ---------

   procedure Set (Field : Timestamp_Field; Value : Times.Timestamp) is
   begin
      Config_Data.Timestamp_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Field : Timestamp_Field) return Times.Timestamp
   is (Config_Data.Timestamp_Fields (Field));

   ---------
   -- Set --
   ---------

   procedure Set (Field : Duration_Field; Value : Times.Duration) is
   begin
      Config_Data.Duration_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Field : Duration_Field) return Times.Duration
   is (Config_Data.Duration_Fields (Field));


   ---------
   -- Set --
   ---------

   procedure Set (Field : Boolean_Field; Value : Boolean) is
   begin
      Config_Data.Boolean_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Field : Boolean_Field) return Boolean
   is (Config_Data.Boolean_Fields (Field));


begin
   if (for some F in Configuration_Field => Is_Set (F)) then
      -- We should nevee arrive here
      raise Program_Error;
   end if;
end Config.Data;
