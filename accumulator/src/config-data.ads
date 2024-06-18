with Times;
with Config.Syntax;

private package Config.Data with SPARK_Mode is
   use type Memory_Dynamic.Dynamic_Type;
   use type Images.Pixel_Value;
   use type Times.Timestamp;
   use type Times.Duration;

   type Configuration_Field is
     (
      Decay,
      Output_Filename_Template,
      Verbosity_Level,
      First_Image,
      -- String fields
      Input,
      Log_Progress,
      Metadata_Filename,
      -- Numeric fields
      Min,
      Max,
      Neutral,
      Sampling_Period,
      Start_Time,
      Stop_Time,
      Event_Weigth,
      -- Boolean fields
      Negative_Event_Handling,
      Lazy_Decay
     );

   type Start_Image_Class is (Uniform, External);

   type Start_Image_Spec_Type (Class : Start_Image_Class := Uniform) is
      record
         case Class is
            when Uniform =>
               Level : Images.Pixel_Value;

            when External =>
               Filename : Unbounded_String;
         end case;
      end record;


   subtype String_Field is Configuration_Field range Input .. Metadata_Filename;

   subtype Numeric_Field is Configuration_Field range Min .. Event_Weigth;

   subtype Boolean_Field is Configuration_Field range Lazy_Decay .. Lazy_Decay;

   subtype Duration_Field is Configuration_Field range Sampling_Period .. Sampling_Period;

   subtype Timestamp_Field is Configuration_Field range Start_Time .. Stop_Time;

   function Is_Set (Field : Configuration_Field) return Boolean;

   function Is_All_Set return Boolean
   is (for all F in Configuration_Field => Is_Set (F));

   --  procedure Set_Input_Filename (Item : String)
   --    with
   --      Pre => not Is_Set (Input),
   --      Post => Is_Set (Input);
   --
   --  function Input_Filename return String
   --    with
   --      Pre => Is_Set (Input);


   procedure Set_Verbosity_Level (Verb : Verbosity)
     with
       Pre => not Is_Set (Verbosity_level),
       Post => Is_Set (Verbosity_level);

   function Verbosity_Level return Verbosity
     with
       Pre => Is_Set (Verbosity_level);


   procedure Set_Output_Filename_Template (Item : Syntax.Radix_Spec)
     with
       Pre => not Is_Set (Output_Filename_Template),
       Post => Is_Set (Output_Filename_Template);

   function Output_Filename_Template return Syntax.Radix_Spec
     with
       Pre => Is_Set (Output_Filename_Template);

   procedure Set_Negative_Event_Action (Action : Negative_Event_Action)
     with
       Pre => not Is_Set (Negative_Event_Handling),
       Post => Is_Set (Negative_Event_Handling) and then Get_Negative_Event_Action = Action;

   function Get_Negative_Event_Action return Negative_Event_Action
     with
       Pre => not Is_Set (Negative_Event_Handling);

   procedure Set_First_Image_Spec (Spec : Start_Image_Spec_Type)
     with
       Pre => not Is_Set (First_Image),
       Post => Is_Set (First_Image) and then Get_First_Image_Spec = Spec;

   function get_first_image_spec return Start_Image_Spec_Type
     with
       Pre => Is_Set (First_Image);

   procedure Set_Decay (Item : Memory_Dynamic.Dynamic_Type)
     with
       Pre => not Is_Set (Decay),
       Post => Is_Set (Decay) and then Decay = Item;

   function Decay return Memory_Dynamic.Dynamic_Type
     with
       pre => Is_Set (Decay);

   procedure Set (Field : String_Field;
                  Value : String)
     with
       Pre => not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : String_Field) return String
     with
       Pre => Is_Set (Field);


   procedure Set (Field : Numeric_Field;
                  Value : Images.Pixel_Value)
     with
       Pre => not Is_Set (Field),
       Post => (Is_Set (Field) and then Get (Field) = Value);

   function Get (Field : Numeric_Field) return Images.Pixel_Value
     with
       Pre => Is_Set (Field);

   procedure Set (Field : Timestamp_Field;
                  Value : Times.Timestamp)
     with
       Pre =>  not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   procedure Update  (Field : Timestamp_Field;
                      Value : Times.Timestamp)
     with
       Pre =>  Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : Timestamp_Field) return Times.Timestamp
     with
       Pre => Is_Set (Field);

   procedure Set (Field : Duration_Field;
                  Value : Times.Duration)
     with
       Pre =>  not Is_Set (Field),
       Post => Is_Set (Field) and then Get (Field) = Value;

   function Get (Field : Duration_Field) return Times.Duration
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
 end Config.Data;
