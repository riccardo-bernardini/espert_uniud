with DVAccum.Timestamps;

private package DVAccum.Config.Data with SPARK_Mode is
   use type Timestamps.Timestamp;
   use type Timestamps.Duration;

   type Configuration_Field is
     (
      Verbosity_Level,
      First_Image,
      -- String fields
      Filter_Spec,
      Input_Filename,
      Output_Filename_Template,
      Log_Progress,
      -- Numeric fields
      Min,
      Max,
      Neutral,
      Positive_Value,
      Negative_Value,
      Event_Weigth,
      -- Timestamp fields
      Start_Time,
      Stop_Time,
      -- Duration
      Sampling_Period,
      -- Integer fields
      Oversampling,
      N_Tasks,
      -- Fake Boolean field.  Old fields have been removed
      Boolean_Placekeeper
     );

   type Start_Image_Class is (Uniform, External);

   type Start_Image_Spec_Type (Class : Start_Image_Class := Uniform) is
      record
         case Class is
            when Uniform =>
               Level : Sample_Value;

            when External =>
               Filename : Unbounded_String;
         end case;
      end record;


   subtype String_Field is Configuration_Field range Filter_Spec .. Log_Progress;

   subtype Numeric_Field is Configuration_Field range Min .. Event_Weigth;

   subtype Timestamp_Field is Configuration_Field range Start_Time .. Stop_Time;

   subtype Duration_Field is Configuration_Field range Sampling_Period .. Sampling_Period;

   subtype Integer_Field is Configuration_Field range Oversampling .. N_Tasks;

   subtype Boolean_Field is Configuration_Field range Boolean_Placekeeper .. Boolean_Placekeeper;

   function Is_Set (Field : Configuration_Field) return Boolean;

   function Is_All_Set return Boolean
   is (for all F in Configuration_Field => Is_Set (F));


   procedure Add_Input_Filename (Filename : String)
     with
       Post => Is_Set (Input_Filename) and N_Inputs = N_Inputs'Old + 1;


   function N_Inputs return Natural
     with
       Post => (N_Inputs'Result = 0) = (not Is_Set (Input_Filename));

   function Get_Input_Filename (N : Positive) return String
     with
       Pre => N <= N_Inputs;


   procedure Set_Verbosity_Level (Verb : Verbosity)
     with
       Pre => not Is_Set (Verbosity_Level),
       Post => Is_Set (Verbosity_Level);

   function Verbosity_Level return Verbosity
     with
       Pre => Is_Set (Verbosity_Level);


   procedure Set_First_Image_Spec (Spec : Start_Image_Spec_Type)
     with
       Pre => not Is_Set (First_Image),
       Post => Is_Set (First_Image) and then Get_First_Image_Spec = Spec;

   function Get_First_Image_Spec return Start_Image_Spec_Type
     with
       Pre => Is_Set (First_Image);


   procedure Set (Field : String_Field;
                  Value : String)
     with
       Pre => not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : String_Field) return String
     with
       Pre => Is_Set (Field);


   procedure Set (Field : Numeric_Field;
                  Value : Sample_Value)
     with
       Pre => not Is_Set (Field),
       Post => (Is_Set (Field) and then Get (Field) = Value);

   function Get (Field : Numeric_Field) return Sample_Value
     with
       Pre => Is_Set (Field);

   procedure Set (Field : Integer_Field;
                  Value : Integer)
     with
       Pre => not Is_Set (Field),
       Post => (Is_Set (Field) and then Get (Field) = Value);

   function Get (Field : Integer_Field) return Integer
     with
       Pre => Is_Set (Field);

   procedure Set (Field : Timestamp_Field;
                  Value : Timestamps.Timestamp)
     with
       Pre =>  not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   procedure Update  (Field : Timestamp_Field;
                      Value : Timestamps.Timestamp)
     with
       Pre =>  Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : Timestamp_Field) return Timestamps.Timestamp
     with
       Pre => Is_Set (Field);

   procedure Set (Field : Duration_Field;
                  Value : Timestamps.Duration)
     with
       Pre =>  not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : Duration_Field) return Timestamps.Duration
     with
       Pre => Is_Set (Field);

   procedure Set (Field : Boolean_Field;
                  Value : Boolean)
     with
       Pre => not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : Boolean_Field) return Boolean
     with
       Pre => Is_Set (Field);
end DVAccum.Config.Data;
