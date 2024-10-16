with Ada.Finalization;
with Ada.Iterator_Interfaces;

with DVAccum.Frames;

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
     new Finalization.Controlled
   with
     private
       with
         Constant_Indexing => Value;

   type Pixel_Buffer_Access is
     access Pixel_Buffer;


   type Pixel_Index is private;

   type Pixel_Descriptor is
      record
         Location : Frames.Point_Type; -- Pixel coordinate
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


   type Pixel_History is
     array (Natural range <>) of Frames.Pixel_Value;

   function Create (N_Frames, N_Pixels : Positive)
                    return Pixel_Buffer;

   procedure Store (Buffer : in out Pixel_Buffer;
                    Pixel  : Frames.Point_Type;
                    Data   : Pixel_History);

   function Next_Unprocessed_Frame (Buffer : Pixel_Buffer)
                                    return Frame_Index;

   function Value (Buffer : Pixel_Buffer;
                   Pixel  : Pixel_Index;
                   Time   : Frame_Index)
                   return Frames.Pixel_Value;

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
   type Pixel_Index is new Natural;


   type Pixel_List is
     array (Pixel_Index range <>) of Frames.Point_Type;

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
   private
      First_Free : Pixel_Index := Table'First;
   end Pixel_Table_Allocator;

   type Pixel_Allocator_Access is access Pixel_Table_Allocator;


   protected type Frame_Number_Dispenser (N_Frames : Positive)
   is
      procedure Next_Frame (N : out Frame_Index);
   private
      Next : Frame_Index := 0;
   end Frame_Number_Dispenser;

   type Frame_Dispenser_Access is access Frame_Number_Dispenser;

   type Pixel_Array is
     array (Valid_Frame_Index range <>) of Frames.Pixel_Value;

   type Pixel_Array_Access is access Pixel_Array;

   type Pixel_Buffer is
     new Finalization.Controlled
   with
      record
         Pixels          : Pixel_List_Access;
         Values          : Pixel_Array_Access;
         Allocator       : Pixel_Allocator_Access;
         Frame_Dispenser : Frame_Dispenser_Access;
      end record;

   overriding procedure Finalize (Object : in out Pixel_Buffer);

end Dvaccum.Event_Processing.Pixel_Buffers;
