package DVAccum is
type Sample_Value is new Float;

   type Coord_X is mod 2 ** 16;
   type Coord_Y is mod 2 ** 16;

   type Point_Type is
      record
         X : Coord_X;
         Y : Coord_Y;
      end record;
end DVAccum;
