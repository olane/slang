
% should print 1 1
begin
  set n := 10; 
  set x := 1; 
  set y := 20;
  if (n >= x && y >= n) then print 1 else print 0;
  if (x >= n || y >= x) then print 1 else print 0 
end 

