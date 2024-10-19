with Dvaccum.Filters;
with Ada.Text_IO; use Ada.Text_IO;

procedure Dvaccum.Filter_Test
is
   use Filters;

   --  F : constant Filter_Type :=
   --        Parse ("1.0 2.0 3.5 + 1.1 2.1 / 1.1 -1.0 + #1.9s", 1.0);

   G : Filter_Type :=
         Parse ("1.0 / 1.0 -0.5", 1.0);

   X : constant Signal := (0 => 1.0, 1 => 0.0, 2 => 0.0, 3 => 0.0, 4 => 0.0);

   Y : constant Signal := Apply (G, X);
begin
   --  Dump (F);

   Dump (G);

   for C of Y loop
      Put(C'Image & "  ");
   end loop;

   New_Line;
end Dvaccum.Filter_Test;
