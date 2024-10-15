with Ada.Strings.Fixed;

with Tokenize;
with Patterns;

package body Dvaccum.Filters is
   function Parse (Descr    : String;
                   Sampling : Float) return Filter_Type
   is
      use Tokenize;
      use Ada.Strings;

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
            use Frames;

            Coeffs : constant String_Vectors.Vector := Split (X);
         begin
            return Result : Signal (0 .. Integer (Coeffs.Length) - 1) do
               for I in Result'Range loop
                  declare
                     C : constant String := Strip (Coeffs (I + Coeffs.First_Index));
                  begin
                     if not Patterns.Is_Float (C) then
                        raise Constraint_Error;
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
            use Frames;

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
                  Den : constant Signal := Parse_Poly (Input (Input'First .. Last));
                  Tau : constant Float := Mult * Float (Den (Den'First));
               begin
                  if Den'Length /= 1 then
                     raise Constraint_Error;
                  end if;

                  return Pixel_Value (Sampling / Tau);
               end;

            end Tau_To_Pole;

            Parts : constant String_Vectors.Vector := Split (X, '@');
         begin
            if Parts.Length /= 2 then
               raise Constraint_Error;
            end if;

            declare
               Num  : constant Signal := Parse_Poly (Parts (1));
               Pole : constant Pixel_Value := Tau_To_Pole (Strip (Parts (2)), Sampling);
            begin
               return Filter_Atom'
                 (Num_Degree  => 0,
                  Den_Degree  => 1,
                  Status_Size => 1,
                  Num         => Num,
                  Den         => (1.0, -Pole),
                  Status      => (others => 0.0));
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
               return Filter_Atom'
                 (Num_Degree  => Num'Last,
                  Den_Degree  => Den'Last,
                  Status_Size => Integer'Max (Num'Length, Den'Length),
                  Num         => Num,
                  Den         => Den,
                  Status      => (others => 0.0));
            end;
         end Parse_Iir;

         function Parse_Fir (X : String) return Filter_Atom
         is
            Num : constant Signal := Parse_Poly (X);
         begin
            pragma Assert (Num'First = 0);

            return Filter_Atom'(Num_Degree  => Num'Last,
                                Den_Degree  => 0,
                                Status_Size => Num'Length,
                                Num         => Num,
                                Den         => (0 => 0.0),
                                Status      => (others => 0.0));
         end Parse_Fir;

         type Filter_Class is (FIR, IIR, Time_Constant);

         function Classify (X : String) return Filter_Class
         is
            use Ada.Strings.Fixed;
         begin
            if Index (X, "/") /= 0 then
               return IIR;

            elsif Index (X, "@") /= 0 then
               return Time_Constant;

            else
               return FIR;
            end if;
         end Classify;
      begin
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
   begin
      pragma Compile_Time_Warning (True, "Unimplemented");
      raise Program_Error;
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
