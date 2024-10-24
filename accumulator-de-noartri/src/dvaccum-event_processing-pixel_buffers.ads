with Ada.Finalization;
with Ada.Iterator_Interfaces;


use Ada;

--
--  The accumulator tasks produce the "history" of each pixel that experienced some
--  event.  The data produced by the accumulator is indixed by the pair (pixel, time)
--  where pixel=(x,y) belongs to the set of pixels with history and time runs from
--  0 to N_frames-1
--
--  Since the length of the history is always the same, we store the data pixel wise,
--  that is, first the history of pixel p0, then pixel p1, and so on...  Every pixel has
--  an initial location for its history, call it start(pk).  The value of pixel pk at time
--  t is start(pk)+t
--
--  Every frame maker needs to pick a frame number and to cycle on the
--  set of good pixels.
--
--
--
private package Dvaccum.Event_Processing.Pixel_Buffers is

   type Pixel_Buffer is
     new Finalization.Limited_Controlled
   with
     private
       with
         Constant_Indexing => Value;

   type Pixel_Buffer_Access is
     access Pixel_Buffer;


   type Pixel_Index is new Positive;

   type Pixel_Descriptor is
      record
         Location : Point_Type; -- Pixel coordinate
         Index    : Pixel_Index;       -- Used to access the buffer
      end record;

   type Pixel_Cursor is private;

   function Has_Element (Pos : Pixel_Cursor) return Boolean;

   function Element (Pos : Pixel_Cursor) return Pixel_Descriptor
     with
       Pre => Has_Element (Pos);

   package Pixel_Iterators is
     new Ada.Iterator_Interfaces (Pixel_Cursor, Has_Element);

   function Every_Pixel (Buffer : Pixel_Buffer)
                         return Pixel_Iterators.Forward_Iterator'Class;



   function Create (First_Frame : Frame_Index;
                    Last_Frame  : Frame_Index;
                    N_Pixels    : Positive)
                    return Pixel_Buffer_Access
     with
       Pre =>
         Last_Frame >= First_Frame,
       Post =>
           Create'Result.N_Frames = Positive (Last_Frame - First_Frame + 1)
         and Create'Result.N_Pixels = N_Pixels
         and Create'Result.Pixel_Still_Free = N_Pixels;


   function N_Frames (Buffer : Pixel_Buffer) return Positive;
   function Last_Frame (Buffer : Pixel_Buffer) return Frame_Index;
   function First_Frame (Buffer : Pixel_Buffer) return Frame_Index;

   function N_Pixels (Buffer : Pixel_Buffer) return Positive;
   function Last_Pixel (Buffer : Pixel_Buffer) return Pixel_Index;
   function Pixel_Still_Free (Buffer : Pixel_Buffer) return Natural;


   type Pixel_History is
     array (Natural range <>) of Sample_Value;

   procedure Store (Buffer : in out Pixel_Buffer;
                    Pixel  : Point_Type;
                    Data   : Pixel_History)
     with
       Pre =>
         Data'Length = Buffer.N_Frames
         and Buffer.Pixel_Still_Free > 0;

   function Next_Unprocessed_Frame (Buffer : Pixel_Buffer)
                                    return Extended_Frame_Index;

   function Value (Buffer : Pixel_Buffer;
                   Pixel  : Pixel_Index;
                   Time   : Frame_Index)
                   return Sample_Value
     with
       Pre => Time >= Buffer.First_Frame
       and then Time <= Buffer.Last_Frame
       and then Pixel <= Buffer.Last_Pixel;

private
   --
   --  We store the pixel histories in a one-dimensional vector.
   --  The pixel value of pixel px at frame T (T=0,1 ,2, ..)is stored in the
   --  location start(px)+T
   --
   --  We know in advance the length of each history (it is the same for
   --  every pixel) and the number of histories (they are given to the
   --  Create function).  Therefore, we can allocate an array long enough.
   --
   --  At any time there is
   --  * A first unused location, marking the beginning of the free area
   --  * The next frame to be produced
   --

   type Pixel_List is
     array (Pixel_Index range <>) of Point_Type;

   type Pixel_List_Access is access Pixel_List;

   type Pixel_Cursor is
      record
         Cursor     : Pixel_Index;
         Container  : Pixel_List_Access;
      end record;

   type Pixel_Iterator is
     new Pixel_Iterators.Forward_Iterator
   with
      record
         Container : Pixel_List_Access;
      end record;

   overriding
   function First (Object : Pixel_Iterator) return Pixel_Cursor;

   overriding
   function Next (Object : Pixel_Iterator; Position : Pixel_Cursor) return Pixel_Cursor;


   protected type Pixel_Table_Allocator (Table : Pixel_List_Access)
   is
      procedure Next_Free_Entry (Index : out Pixel_Index);

      function Free_Entries return Natural;
   private
      First_Free : Pixel_Index := Table'First;
   end Pixel_Table_Allocator;

   type Pixel_Allocator_Access is access Pixel_Table_Allocator;


   protected type Frame_Number_Dispenser (First_Frame, Last_Frame : Frame_Index)
   is
      procedure Next_Frame (N : out Extended_Frame_Index);
   private
      Next : Frame_Index := First_Frame;
   end Frame_Number_Dispenser;

   type Frame_Dispenser_Access is access Frame_Number_Dispenser;

   subtype Sample_Array is Pixel_History;

   type Sample_Array_Access is access Sample_Array;

   type Pixel_Buffer is
     new Finalization.Limited_Controlled
   with
      record
         First_Frame     : Frame_Index;
         Last_Frame      : Frame_Index;
         N_Pixels        : Positive;
         Pixels          : Pixel_List_Access;
         Samples         : Sample_Array_Access;
         Allocator       : Pixel_Allocator_Access;
         Frame_Dispenser : Frame_Dispenser_Access;
         Store_Call      : Positive;
      end record;

   overriding procedure Finalize (Object : in out Pixel_Buffer);


   function N_Frames (Buffer : Pixel_Buffer) return Positive
   is (Natural (Buffer.Last_Frame - Buffer.First_Frame)+ 1);

   function N_Pixels (Buffer : Pixel_Buffer) return Positive
   is (Buffer.N_Pixels);

   function Last_Frame (Buffer : Pixel_Buffer) return Frame_Index
   is (Buffer.Last_Frame);

   function First_Frame (Buffer : Pixel_Buffer) return Frame_Index
   is (Buffer.First_Frame);


   function Last_Pixel (Buffer : Pixel_Buffer) return Pixel_Index
   is (Pixel_Index (Buffer.N_Pixels - 1) + Pixel_Index'First);

   function Pixel_Still_Free (Buffer : Pixel_Buffer) return Natural
   is (Buffer.Allocator.Free_Entries);


end Dvaccum.Event_Processing.Pixel_Buffers;
