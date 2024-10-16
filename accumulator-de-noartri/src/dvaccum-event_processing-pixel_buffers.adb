pragma Ada_2012;
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

   function Create (N_Frames, N_Pixels : Positive) return Pixel_Buffer is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   -----------
   -- Store --
   -----------

   procedure Store
     (Buffer : in out Pixel_Buffer;
      Pixel  : Frames.Point_Type;
      Data   :        Pixel_History)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Store unimplemented");
      raise Program_Error with "Unimplemented procedure Store";
   end Store;

   ----------------------------
   -- Next_Unprocessed_Frame --
   ----------------------------

   function Next_Unprocessed_Frame (Buffer : Pixel_Buffer) return Frame_Index
   is
      Result : Frame_Index;
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
      return Frames.Pixel_Value
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Value unimplemented");
      return raise Program_Error with "Unimplemented function Value";
   end Value;

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

      procedure Next_Frame (N : out Frame_Index) is
      begin
         if Next > Frame_Index (N_Frames) then
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
         if First_Free > Table'Last then
            raise Constraint_Error;
         end if;

         Index := First_Free;
         First_Free := First_Free + 1;
      end Next_Free_Entry;
   end Pixel_Table_Allocator;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Pixel_Buffer) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Finalize unimplemented");
      raise Program_Error with "Unimplemented procedure Finalize";
   end Finalize;

end Dvaccum.Event_Processing.Pixel_Buffers;
