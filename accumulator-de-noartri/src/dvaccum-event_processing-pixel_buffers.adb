pragma Ada_2012;
package body Dvaccum.Event_Processing.Pixel_Buffers is

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Pos : Pixel_Cursor) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Has_Element unimplemented");
      return raise Program_Error with "Unimplemented function Has_Element";
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element (Pos : Pixel_Cursor) return Pixel_Descriptor is
   begin
      pragma Compile_Time_Warning (Standard.True, "Element unimplemented");
      return raise Program_Error with "Unimplemented function Element";
   end Element;

   -----------------
   -- Every_Pixel --
   -----------------

   function Every_Pixel
     (Buffer : Pixel_Buffer) return Pixel_Iterators.Forward_Iterator'Class
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Every_Pixel unimplemented");
      return raise Program_Error with "Unimplemented function Every_Pixel";
   end Every_Pixel;

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
     (Buffer : in out Pixel_Buffer; Pixel : Frames.Point_Type;
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
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Next_Unprocessed_Frame unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Next_Unprocessed_Frame";
   end Next_Unprocessed_Frame;

   -----------
   -- Value --
   -----------

   function Value
     (Buffer : Pixel_Buffer; Pixel : Frames.Point_Type; Time : Frame_Index)
      return Frames.Pixel_Value
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Value unimplemented");
      return raise Program_Error with "Unimplemented function Value";
   end Value;

   -----------
   -- First --
   -----------

   overriding function First (Object : Pixel_Iterator) return Pixel_Cursor is
   begin
      pragma Compile_Time_Warning (Standard.True, "First unimplemented");
      return raise Program_Error with "Unimplemented function First";
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object : Pixel_Iterator; Position : Pixel_Cursor) return Pixel_Cursor
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Next unimplemented");
      return raise Program_Error with "Unimplemented function Next";
   end Next;

   ----------------------------
   -- Frame_Number_Dispenser --
   ----------------------------

   protected body Frame_Number_Dispenser is

      ----------------
      -- Next_Frame --
      ----------------

      procedure Next_Frame (N : out Frame_Index) is
      begin
         pragma Compile_Time_Warning
           (Standard.True, "Next_Frame unimplemented");
         raise Program_Error with "Unimplemented procedure Next_Frame";
      end Next_Frame;

   end Frame_Number_Dispenser;

   -------------------------
   -- Protected_Allocator --
   -------------------------

   protected body Protected_Allocator is

      --------------
      -- Allocate --
      --------------

      procedure Allocate (Pixel : Frames.Point_Type; Index : out Frame_Index)
      is
      begin
         pragma Compile_Time_Warning (Standard.True, "Allocate unimplemented");
         raise Program_Error with "Unimplemented procedure Allocate";
      end Allocate;

      ----------------
      -- Next_Pixel --
      ----------------

      procedure Next_Pixel
        (Pixel : out Frames.Point_Type; Index : out Frame_Index)
      is
      begin
         pragma Compile_Time_Warning
           (Standard.True, "Next_Pixel unimplemented");
         raise Program_Error with "Unimplemented procedure Next_Pixel";
      end Next_Pixel;

   end Protected_Allocator;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Pixel_Buffer) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Finalize unimplemented");
      raise Program_Error with "Unimplemented procedure Finalize";
   end Finalize;

end Dvaccum.Event_Processing.Pixel_Buffers;
