package DVAccum is
type Sample_Value is new Float;

   type Coord_X is mod 2 ** 16;
   type Coord_Y is mod 2 ** 16;

   type Point_Type is
      record
         X : Coord_X;
         Y : Coord_Y;
      end record;

   type Frame_Index is range 0 .. Integer'Last;

   subtype Extended_Frame_Index
     is Frame_Index'Base range Frame_Index'First - 1 .. Frame_Index'Last;


   No_Frame : constant Extended_Frame_Index := Extended_Frame_Index'First;
end DVAccum;
