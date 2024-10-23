pragma Ada_2012;

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Dvaccum.Event_Processing.Pixel_Buffers is

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Pos : Pixel_Cursor) return Boolean
   is (Pos.Cursor <= Pos.Container'Last);

   -------------
   -- Element --
   -------------

   function Element (Pos : Pixel_Cursor) return Pixel_Descriptor
   is ((Location => Pos.Container (Pos.Cursor),
        Index    => Pos.Cursor));


   -----------------
   -- Every_Pixel --
   -----------------

   function Every_Pixel
     (Buffer : Pixel_Buffer) return Pixel_Iterators.Forward_Iterator'Class
   is (Pixel_Iterator'(Container => Buffer.Pixels));
   ------------
   -- Create --
   ------------

   function Create (First_Frame : Frame_Index;
                    Last_Frame  : Frame_Index;
                    N_Pixels    : Positive)
                    return Pixel_Buffer_Access
   is
      use Ada.Finalization;

      N_Frames : constant Positive := Natural (Last_Frame - First_Frame) + 1;

      Pixels  : constant Pixel_List_Access :=
                  new Pixel_List
                    (Pixel_Index'First .. Pixel_Index (N_Pixels)+Pixel_Index'First - 1);

      Samples : constant Sample_Array_Access :=
                  new Sample_Array (1 .. N_Frames * N_Pixels);

      Allocator : constant Pixel_Allocator_Access :=
                    new Pixel_Table_Allocator (Pixels);

      Frame_Dispenser : constant Frame_Dispenser_Access :=
                          new Frame_Number_Dispenser (First_Frame, Last_Frame);
   begin
      Put_Line ("npixel=" & N_Pixels'Image
                & " nframes=" & N_Frames'Image
                & " last=" & Samples'Last'Image);
      return new Pixel_Buffer'
        (Limited_Controlled
         with
           Pixels           => Pixels,
         Samples          => Samples,
         Allocator        => Allocator,
         Frame_Dispenser  => Frame_Dispenser,
         Store_Call       => 1,
         First_Frame      => First_Frame,
         Last_Frame       => Last_Frame,
         N_Pixels         => N_Pixels);
   end Create;

   function Index_Of (Buffer   : Pixel_Buffer;
                      position : Pixel_Index;
                      Frame    : Frame_Index)
                      return Natural
   is (Natural (Position - Pixel_Index'First) * Buffer.N_Frames
       + Natural (Frame - Buffer.First_Frame)
       + Buffer.Samples'First);

   -----------
   -- Store --
   -----------

   procedure Store
     (Buffer : in out Pixel_Buffer;
      Pixel  :        Point_Type;
      Data   :        Pixel_History)
   is
      Position : Pixel_Index;
   begin

      Buffer.Allocator.Next_Free_Entry (Position);
      Buffer.Pixels (Position) := Pixel;

      Put_Line ("CALL N." & Buffer.Store_Call'Image
                & " position=" & Position'Image & " " & Pixel_Index'First'Image);
      Buffer.Store_Call := Buffer.Store_Call + 1;


      declare
         Start : constant Positive := Index_Of (Buffer, Position, 0);
   --                Natural (Position) * Buffer.N_Frames + Buffer.Samples'First;
      begin
         Put_Line ("GG"
                   & "start=" & Start'Image
                   & " len=" & Data'Length'Image
                   & " first=" & Buffer.Samples'First'Image
                   & " last=" & Buffer.Samples'Last'Image
                   & " start+len=" & Positive'Image (Start + Data'Length)
                  );

         Buffer.Samples (Start .. Start + Data'Length - 1) := Data;
      end;
   end Store;

   ----------------------------
   -- Next_Unprocessed_Frame --
   ----------------------------

   function Next_Unprocessed_Frame (Buffer : Pixel_Buffer)
                                    return Extended_Frame_Index
   is
      Result : Extended_Frame_Index;
   begin
      Buffer.Frame_Dispenser.Next_Frame (Result);

      return Result;
   end Next_Unprocessed_Frame;

   -----------
   -- Value --
   -----------

   function Value
     (Buffer : Pixel_Buffer;
      Pixel  : Pixel_Index;
      Time   : Frame_Index)
      return Sample_Value
   is (Buffer.Samples (Index_Of (Buffer, Pixel, Time)));

   -----------
   -- First --
   -----------

   overriding function First (Object : Pixel_Iterator) return Pixel_Cursor
   is ((Cursor    => Object.Container'First,
        Container => Object.Container));

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object : Pixel_Iterator; Position : Pixel_Cursor) return Pixel_Cursor
   is ((Cursor    => Position.Cursor + 1,
        Container => Position.Container));

   ----------------------------
   -- Frame_Number_Dispenser --
   ----------------------------

   protected body Frame_Number_Dispenser is

      ----------------
      -- Next_Frame --
      ----------------

      procedure Next_Frame (N : out Extended_Frame_Index) is
      begin
         if Next > Last_Frame then
            N := No_Frame;
         else
            N := Next;
            Next := Next + 1;
         end if;
      end Next_Frame;

   end Frame_Number_Dispenser;

   -------------------------
   -- Protected_Allocator --
   -------------------------

   protected body Pixel_Table_Allocator is

      procedure Next_Free_Entry (Index : out Pixel_Index)
      is
      begin
         Put_Line ("NEXT FREE first-free=" & First_Free'Image);

         if First_Free > Table'Last then
            raise Constraint_Error;
         end if;

         Index := First_Free;
         First_Free := First_Free + 1;
      end Next_Free_Entry;

      function Free_Entries return Natural
      is (Natural (Table'Last - First_Free + 1));
   end Pixel_Table_Allocator;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Pixel_Buffer) is
      procedure Free is
        new Ada.Unchecked_Deallocation(Sample_Array, Sample_Array_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation(Frame_Number_Dispenser, Frame_Dispenser_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation(Pixel_Table_Allocator, Pixel_Allocator_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation(Pixel_List, Pixel_List_Access);

   begin
      Free (Object.Allocator);
      Free (Object.Frame_Dispenser);
      Free (Object.Pixels);
      Free (Object.Samples);
   end Finalize;

end Dvaccum.Event_Processing.Pixel_Buffers;
