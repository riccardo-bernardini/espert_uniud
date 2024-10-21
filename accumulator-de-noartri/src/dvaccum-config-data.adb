pragma Ada_2012;

with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package body DVAccum.Config.Data with SPARK_Mode is
   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Set_Fields : array (Configuration_Field) of Boolean := (others => False);

   type String_Field_Array is array (String_Field) of Unbounded_String;

   type Integer_Field_Array is array (Integer_Field) of Integer;

   type Numeric_Field_Array is array (Numeric_Field) of Sample_Value;

   --  type Boolean_Field_Array is array (Boolean_Field) of Boolean;

   type Duration_Field_Array is array (Duration_Field) of Timestamps.Duration;

   type Timestamp_Field_Array is array (Timestamp_Field) of Timestamps.Timestamp;

   --
   --  Why using a record? Wouldn't be it simpler to use just variables?
   --  No special reason, really, just the desire of keeping all the
   --  configuration data together.  Also, with this approach the record
   --  acts as a namespace, simplifying the management of names.
   --
   type Config_Data_Record is
      record
         Requested_Verbosity     : Verbosity;
         Input_Filenames         : String_Vectors.Vector;

         First_Image_Spec        : Start_Image_Spec_Type;

         String_Fields           : String_Field_Array;
         Numeric_Fields          : Numeric_Field_Array;
         Integer_Fields          : Integer_Field_Array;
         Timestamp_Fields        : Timestamp_Field_Array;
         Duration_Fields         : Duration_Field_Array;
      end record;

   Config_Data : Config_Data_Record;


   procedure Dump_Unset
   is
   begin
      for F in Configuration_Field loop
         if not Set_Fields (F) then
            Put_Line (Standard_Error, F'Image);
         end if;
      end loop;
   end Dump_Unset;

   procedure Add_Input_Filename (Filename : String)
   is
   begin
      Config_Data.Input_Filenames.Append (Filename);
      Set_Fields (Input_Filename) := True;
   end Add_Input_Filename;

   function N_Inputs return Natural
   is
   begin
      if False then
         Put_Line (Is_Set (Input_Filename)'Image);
         Put_Line (Config_Data.Input_Filenames.Length'Image);
      end if;

      return Natural (Config_Data.Input_Filenames.Length);
   end N_Inputs;

   function Get_Input_Filename (N : Positive) return String
   is (Config_Data.Input_Filenames (N));

   procedure Is_Set (Field : Configuration_Field)
     with
       Post => Is_Set (Field);

   procedure Is_Set (Field : Configuration_Field)
   is
   begin
      Set_Fields (Field) := True;
   end Is_Set;

   --  procedure Set_Negative_Event_Action (Action : Negative_Event_Action)
   --  is
   --  begin
   --     Config_Data.On_Negative_Event := Action;
   --     Is_Set (Negative_Event_Handling);
   --  end Set_Negative_Event_Action;
   --
   --  function Get_Negative_Event_Action return Negative_Event_Action
   --  is (Config_Data.On_Negative_Event);

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


   procedure Set (Field : Integer_Field;
                  Value : Integer)
   is
   begin
      Config_Data.Integer_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   function Get (Field : Integer_Field) return Integer
   is (Config_Data.Integer_Fields (Field));

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

   procedure Set (Field : Numeric_Field; Value : Sample_Value) is
   begin
      Config_Data.Numeric_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ------------
   -- Update --
   ------------

   procedure Update  (Field : Timestamp_Field;
                      Value : Timestamps.Timestamp)is
   begin
      Config_Data.Timestamp_Fields (Field) := Value;
   end Update;


   ---------
   -- Get --
   ---------

   function Get (Field : Numeric_Field) return Sample_Value
   is (Config_Data.Numeric_Fields (Field));

   ---------
   -- Set --
   ---------

   procedure Set (Field : Timestamp_Field; Value : Timestamps.Timestamp) is
   begin
      Config_Data.Timestamp_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Field : Timestamp_Field) return Timestamps.Timestamp
   is (Config_Data.Timestamp_Fields (Field));

   ---------
   -- Set --
   ---------

   procedure Set (Field : Duration_Field; Value : Timestamps.Duration) is
   begin
      Config_Data.Duration_Fields (Field) := Value;
      Is_Set (Field);
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Field : Duration_Field) return Timestamps.Duration
   is (Config_Data.Duration_Fields (Field));


   --  ---------
   --  -- Set --
   --  ---------
   --
   --  procedure Set (Field : Boolean_Field; Value : Boolean) is
   --  begin
   --     Config_Data.Boolean_Fields (Field) := Value;
   --     Is_Set (Field);
   --  end Set;
   --
   --  ---------
   --  -- Get --
   --  ---------
   --
   --  function Get (Field : Boolean_Field) return Boolean
   --  is (Config_Data.Boolean_Fields (Field));


begin
   if (for some F in Configuration_Field => Is_Set (F)) then
      -- We should nevee arrive here
      raise Program_Error;
   end if;
end DVAccum.Config.Data;
