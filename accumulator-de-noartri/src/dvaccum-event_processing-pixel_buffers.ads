with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Iterator_Interfaces;

with DVAccum.Frames;
with DVAccum.Timestamps;

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
--  Every frame maker needs to pick a frame number and the cicle on the set of good
--  pixels.
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


   type Pixel_ID is private;

   type Pixel_Descriptor is
      record
         Location : Frames.Point_Type;
         Index    : Pixel_ID;
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


   type Pixel_History is array (Natural range <>) of Frames.Pixel_Value;

   function Create (N_Frames, N_Pixels : Positive)
                    return Pixel_Buffer;

   procedure Store (Buffer : in out Pixel_Buffer;
                    Pixel  : Frames.Point_Type;
                    Data   : Pixel_History);

   function Next_Unprocessed_Frame (Buffer : Pixel_Buffer)
                                    return Frame_Index;

   function Value (Buffer : Pixel_Buffer;
                   Pixel  : Frames.Point_Type;
                   Time   : Frame_Index)
                   return Frames.Pixel_Value;

private
   type Pixel_ID is new Natural;



   package Pixel_Data_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Pixel_Descriptor);

   type Pixel_Cursor is new Pixel_Data_Lists.Cursor;

   type Pixel_Iterator is
     new Pixel_Iterators.Forward_Iterator
   with
      record
         Cursor : Pixel_Cursor;
      end record;

   overriding
   function First (Object : Pixel_Iterator) return Pixel_Cursor;

   overriding
   function Next (Object : Pixel_Iterator; Position : Pixel_Cursor) return Pixel_Cursor;

   protected type Frame_Number_Dispenser (N_Frames : Positive)
   is
      procedure Next_Frame (N : out Frame_Index);
   private
      Next : Frame_Index := 0;
   end Frame_Number_Dispenser;

   type Frame_Dispenser_Access is access Frame_Number_Dispenser;

   protected type Protected_Allocator (N_Frames : Positive)
   is
      procedure Allocate (Pixel : Frames.Point_Type;
                          Index : out Frame_Index)
        with
          Post => Index in Valid_Frame_Index;

      procedure Next_Pixel (Pixel : out Frames.Point_Type;
                            Index : out Frame_Index);
   private
      Allocation_Table : Pixel_Data_Lists.List;

      Next_To_Process : Pixel_Data_Lists.Cursor := Pixel_Data_Lists.No_Element;

      First_Free : Frame_Index := Frame_Index'First;
   end Protected_Allocator;

   type Protected_Allocator_Access is access Protected_Allocator;

   type Pixel_Array is
     array (Valid_Frame_Index range <>) of Frames.Pixel_Value;

   type Pixel_Array_Access is access Pixel_Array;

   type Pixel_Buffer is
     new Finalization.Controlled
   with
      record
         Allocator       : Protected_Allocator_Access;
         Pixels          : Pixel_Array_Access;
         Frame_Dispenser : Frame_Dispenser_Access;
      end record;

   overriding procedure Finalize (Object : in out Pixel_Buffer);

end Dvaccum.Event_Processing.Pixel_Buffers;
