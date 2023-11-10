with Ada.Strings.Fixed;
with Gnat.Regpat;

package body Time_Syntax is
   function Parse_Time_Spec (Spec : String) return Time_In_Microseconds
   is
      Success  : Boolean;
      Relative : Boolean;
      Value    : Time_In_Microseconds;
   begin
      Parse_Time (Input    => Spec,
                  Success  => Success,
                  Relative => Relative,
                  Value    => Value);

      if (not Success) or else (Relative or Value < 0.0) then
         raise Constraint_Error with "Bad time spec";
      end if;

      return Value;
   end Parse_Time_Spec;
   pragma Unreferenced (Parse_Time_Spec);

   procedure Parse_Time (Input    : String;
                         Success  : out Boolean;
                         Relative : out Boolean;
                         Value    : out Time_In_Microseconds)
   is
      use Gnat.Regpat;

      Time_Regexp : constant Pattern_Matcher :=
                      Compile ("^ *(@\+)?"
                               & "(^[0-9_]+)"
                               & "(\.[0-9_]+(?:[eE][-+]?[0-9_]+)?)?"
                               & " *"
                               & "([a-z]+)? *$");

      Matches     : Match_Array (0 .. Paren_Count (Time_Regexp));

      All_Regexp         : constant := 0;
      Relative_Section   : constant := 1;
      Integer_Section    : constant := 2;
      Fractional_Section : constant := 3;
      Unit_Section       : constant := 4;

      function Section (N : Integer) return String
      is (Input (Matches (N).First .. Matches (N).Last))
        with
          Pre => Matches (N) /= No_Match;

      function Found (Section : Integer) return Boolean
      is (Matches (Section) /= No_Match);

      function Parse_Number return Float
        with
          Pre => Found (Integer_Section);

      function Parse_Number return Float
      is
      begin
         if not Found (Fractional_Section)  then
            return Float (Integer'Value (Section (Integer_Section)));

         else
            declare
               Full_Number : constant String :=
                               Section (Integer_Section) & Section (Fractional_Section);
            begin
               return Float'Value (Full_Number);
            end;
         end if;
      end Parse_Number;

      function Get_Unit return String
      is (if not Found (Unit_Section) then "" else Section (Unit_Section));

      function Apply_Unit (Value : Float; Unit : String) return Time_In_Microseconds
      is
         subtype Unit_Name is String (1 .. 2);

         type Unit_Entry is
            record
               Name  : Unit_Name;
               Value : Float;
            end record;

         Unit_Table : constant array (1 .. 5) of Unit_Entry :=
                        (("  ", 1.0),
                         (" s", 1.0e6),
                         (Name => "ms", Value => 1.0e3),
                         (Name => "us", Value => 1.0),
                         (Name => "ns", Value => 1.0e-3));
      begin
         if Unit = "fps" then
            return Time_In_Microseconds (1.0e6 / Value);
         end if;

         if Unit'Length > 2 then
            raise Constraint_Error with "Unknown unit '" & Unit & "'";
         end if;

         declare
            use Ada.Strings.Fixed;

            Padded : constant Unit_Name := ((2 - Unit'Length) * " ") & Unit;
         begin
            for Table_Entry of Unit_Table loop
               if Padded = Table_Entry.Name then
                  return Time_In_Microseconds (Table_Entry.Value * Value);
               end if;
            end loop;
         end;

         raise Constraint_Error with "Unknown unit '" & Unit & "'";
      end Apply_Unit;

   begin
      Gnat.Regpat.Match (Self       => Time_Regexp,
                         Data       => Input,
                         Matches    => Matches);


      if
        (Matches (All_Regexp) = No_Match)
        or else (not Found (Fractional_Section)  and Found (Unit_Section))
      then
         Success := False;
         Relative := False;
         Value := 0.0;
         return;
      end if;

      pragma Assert (Matches (Integer_Section) /= No_Match);

      Relative := Found (Relative_Section);

      Value := Apply_Unit (Parse_Number, Get_Unit);
   end Parse_Time;

   -------------------
   -- Is_Valid_Time --
   -------------------

   function Is_Valid_Time (Spec : String) return Boolean
   is
      Success  : Boolean;
      Relative : Boolean;
      Value    : Time_In_Microseconds;
   begin
      Parse_Time (Input    => Spec,
                  Success  => Success,
                  Relative => Relative,
                  Value    => Value);

      return Success and then (not Relative and Value >= 0.0);
   end Is_Valid_Time;
   pragma Unreferenced (Is_Valid_Time);





end Time_Syntax;
