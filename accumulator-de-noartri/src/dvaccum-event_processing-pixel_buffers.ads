with Ada.Containers.Doubly_Linked_Lists;

with DVAccum.Frames;
with DVAccum.Timestamps;

private package Dvaccum.Event_Processing.Pixel_Buffers is
   type Pixel_Index is private;

   No_Pixel : constant Pixel_Index;

   type Sample_Segment is array (Natural range <>) of Frames.Pixel_Value;

   type Pixel_Buffer is private;

   function Create (N_Frames, N_Pixels : Positive)
                    return Pixel_Buffer;

   procedure Destroy (Buffer : in out Pixel_Buffer);

   procedure Store (Buffer : in out Pixel_Buffer;
                    Pixel  : Frames.Point_Type;
                    Data   : Sample_Segment);

   procedure Next_Pixel (Buffer : in out Pixel_Buffer;
                         Pixel  : out Frames.Point_Type;
                         Index  : out Pixel_Index);

   function Value (Buffer : Pixel_Buffer;
                   Index  : Pixel_Index;
                   Time   : Timestamps.Timestamp)
                   return Frames.Pixel_Value;

private
   type Pixel_Index is new Natural;

   subtype Valid_Pixel_Index is
     Pixel_Index range Pixel_Index'First + 1 .. Pixel_Index'Last;

   No_Pixel : constant Pixel_Index := Pixel_Index'First;

   type Pixel_Data is
      record
         Location : Frames.Point_Type;
         Index    : Valid_Pixel_Index;
      end record;

   package Pixel_Data_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Pixel_Data);

   protected type Protected_Allocator (N_Frames : Positive)
   is
      procedure Allocate (Pixel : Frames.Point_Type;
                          Index : out Pixel_Index)
        with
          Post => Index in Valid_Pixel_Index;

      procedure Next_Pixel (Pixel : out Frames.Point_Type;
                            Index : out Pixel_Index);
   private
      Allocation_Table : Pixel_Data_Lists.List;

      Next_To_Process : Pixel_Data_Lists.Cursor := Pixel_Data_Lists.No_Element;

      First_Free : Pixel_Index := Pixel_Index'First;
   end Protected_Allocator;

   type Protected_Allocator_Access is access Protected_Allocator;

   type Pixel_Array is
     array (Valid_Pixel_Index range <>) of Frames.Pixel_Value;

   type Pixel_Array_Access is access Pixel_Array;

   type Pixel_Buffer is
      record
         Allocator : Protected_Allocator_Access;
         Pixels : Pixel_Array_Access;
      end record;

end Dvaccum.Event_Processing.Pixel_Buffers;
