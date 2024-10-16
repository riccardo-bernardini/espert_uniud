with Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions;

with Tokenize;
with Patterns;

package body Dvaccum.Filters is

   function "*" (Gain : Frames.Pixel_Value;
                 Filter : Filter_Type)
                 return Filter_Type
   is
      use type Frames.Pixel_Value;
   begin
      return Result : Filter_Type := Filter do
         for Atom of Result.Atoms loop
            for C of Atom.Num loop
               C := C * Gain;
            end loop;
         end loop;
      end return;
   end "*";

   ---------------
   -- Make_Atom --
   ---------------

   function Make_Atom (Num, Den : Signal)
                       return Filter_Atom
   is
      Max_Deg : constant Natural := Natural'Max (Num'Last, Den'Last);

   begin
      return Result : Filter_Atom (Max_Deg) do
         Result.Den (Den'Range) := Den;
         Result.Num (Num'Range) := Num;

         Result.Is_Fir := (Den'Length = 1);
      end return;
   end Make_Atom;

   function Parse (Descr    : String;
                   Sampling : Float) return Filter_Type
   is
      use Tokenize;
      use Ada.Strings;
      use Frames;

      function Strip (S : String) return String
      is (Fixed.Trim (Source => S,
                      Side   => Both));


      function Parse_Atom (Input    : String;
                           Sampling : Float)
                           return Filter_Atom
      is
         function Parse_Poly (X : String) return Signal
           with
             Post => Parse_Poly'Result'First = 0;

         function Parse_Poly (X : String) return Signal
         is

            Coeffs : constant String_Vectors.Vector := Split (X);
         begin
            return Result : Signal (0 .. Integer (Coeffs.Length) - 1) do
               for I in Result'Range loop
                  declare
                     C : constant String := Strip (Coeffs (I + Coeffs.First_Index));
                  begin
                     if not Patterns.Is_Float (C) then
                        raise Parsing_Error
                          with "Invalid number [" & C & "]";
                     end if;

                     Result (I) := Pixel_Value'Value (C);
                  end;
               end loop;
            end return;
         end Parse_Poly;

         function Parse_Time_Constant (X        : String;
                                       Sampling : Float)
                                       return Filter_Atom
         is
            use type Ada.Containers.Count_Type;

            function Tau_To_Pole (Input    : String;
                                  Sampling : Float)
                                  return Frames.Pixel_Value
            is
               Mult : Float;
               Last : Positive;
            begin
               if Input'Length < 3 or else Input (Input'Last) /= 's' then
                  raise Constraint_Error;
               end if;

               if Input (Input'Last - 1) = 'm' then
                  Mult := 1.0e-3;
                  Last := Input'Last - 2;

               elsif Input (Input'Last - 1) = 'u' then
                  Mult := 1.0e-6;
                  Last := Input'Last - 2;

               else
                  Mult := 1.0;
                  Last := Input'Last - 1;

               end if;


               declare
                  use Ada.Numerics.Elementary_Functions;

                  Number : constant String := Input (Input'First .. Last);
                  Tau : Float;
               begin
                  if not Patterns.Is_Float (Number) then
                     raise Parsing_Error
                       with "Invalid number [" & Number & "]";
                  end if;

                  Tau := Mult * Float'Value (Number);
                  return Pixel_Value (Exp (-Sampling / Tau));
               end;

            end Tau_To_Pole;

            Parts : constant String_Vectors.Vector := Split (X, '#');
         begin
            if Parts.Length /= 2 then
               raise Constraint_Error;
            end if;

            declare
               Num_Part  : constant String := Strip (Parts (1));
               Num       : Pixel_Value;
               Pole      : constant Pixel_Value := Tau_To_Pole (Strip (Parts (2)), Sampling);
            begin
               if Num_Part = "" then
                  Num := 1.0;

               elsif Patterns.Is_Float (Num_Part) then
                  Num := Pixel_Value'Value (Num_Part);

               else
                  raise Constraint_Error;

               end if;

               return Make_Atom (Num => (0 => Num),
                                 Den => (1.0, -Pole));

            end;
         end Parse_Time_Constant;

         function Parse_Iir (X : String) return Filter_Atom
         is
            use type Ada.Containers.Count_Type;

            Parts : constant String_Vectors.Vector := Split (X, '/');
         begin
            if Parts.Length /= 2 then
               raise Constraint_Error;
            end if;

            declare
               Num : constant Signal := Parse_Poly (Parts (1));
               Den : constant Signal := Parse_Poly (Parts (2));
            begin
               return Make_Atom (Num => Num, Den => Den);
            end;
         end Parse_Iir;

         function Parse_Fir (X : String) return Filter_Atom
         is
            Num : constant Signal := Parse_Poly (X);
         begin
            pragma Assert (Num'First = 0);

            return Make_Atom (Num => Num, Den => (0 => 1.0));
         end Parse_Fir;

         type Filter_Class is (FIR, IIR, Time_Constant);

         function Classify (X : String) return Filter_Class
         is
            use Ada.Strings.Fixed;
         begin
            if Index (X, "/") /= 0 then
               return IIR;

            elsif Index (X, "#") /= 0 then
               return Time_Constant;

            else
               return FIR;
            end if;
         end Classify;
      begin
         if Input = "integrator"
           or Input = "int"
           or Input = "accumulator"
           or Input = "acc"
         then
            return Make_Atom (Num => (0 => 1.0),
                              Den => (0 => 1.0, 1 => -1.0));
         end if;

         case Classify (Input) is
            when FIR =>
               return Parse_FIR (Input);

            when IIR =>
               return Parse_IIR (Input);

            when Time_Constant =>
               return Parse_Time_Constant (Input, Sampling);

         end case;
      end Parse_Atom;

      Atom_Descr : constant String_Vectors.Vector := Split (Descr, '+');
   begin
      return Result : Filter_Type := (Atoms => Atom_Vectors.Empty_Vector) do
         for D of Atom_Descr loop
            Result.Atoms.Append (Parse_Atom (D, Sampling));
         end loop;
      end return;
   end Parse;
   -----------
   -- Reset --
   -----------

   procedure Reset (Filter : in out Filter_Type)
   is
   begin
      for Atom of Filter.Atoms loop
         Atom.Status := (others => 0.0);
      end loop;
   end Reset;

   -------------
   -- Process --
   -------------

   procedure Process (Filter : in out Filter_Atom;
                      Output :    out Frames.Pixel_Value;
                      Input  :        Frames.Pixel_Value)
   is
      use type Frames.Pixel_Value;
   begin
      if Filter.Is_FIR then
         Output := Input * Filter.Num (0)+Filter.Status (1);

         for I in 1 .. Filter.Degree - 1 loop
            Filter.Status (I) :=
              Filter.Status (I + 1) +
              Input * Filter.Num (I);
         end loop;

         Filter.Status (Filter.Degree) :=
           Input * Filter.Num (Filter.Degree);
      else
         Output := Input * Filter.Num (0)+Filter.Status (1);

         for I in 1 .. Filter.Degree - 1 loop
            Filter.Status (I) :=
              Filter.Status (I + 1) +
              Input * Filter.Num (I) +
              Output * Filter.Den (I);
         end loop;

         Filter.Status (Filter.Degree) :=
           Input * Filter.Num (Filter.Degree) +
           Output * Filter.Den (Filter.Degree);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Filter : in out Filter_Type;
                      Output :    out Frames.Pixel_Value;
                      Input  :        Frames.Pixel_Value)
   is
      use Frames;

      Buffer : Pixel_Value;
   begin
      Output := 0.0;

      for Atom of Filter.Atoms loop
         Process (Atom, Buffer, Input);

         Output := Output + Buffer;
      end loop;
   end Process;


   function Apply (Filter : in out Filter_Type;
                   Input  : Signal)
                   return Signal
   is
      Result : Signal (Input'Range);
   begin
      Reset (Filter);

      for I in Input'Range loop
         Process (Filter, Result (I), Input (I));
      end loop;

      return Result;
   end Apply;
end Dvaccum.Filters;
