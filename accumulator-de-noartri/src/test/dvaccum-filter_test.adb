with Dvaccum.Filters;

procedure Dvaccum.Filter_Test
is
   use Filters;

   F : constant Filter_Type := Parse("1.0 2.0 3.5 + 1.1 2.1 / 1.1 -1.0 + #1.9s", 1.0);
begin
   Dump (F);
end Dvaccum.Filter_Test;
