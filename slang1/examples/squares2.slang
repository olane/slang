
% just squares.slang with while condition 
% expressed in a different way 
begin
  set n := 10; 
  set x := 1; 
  while ~(x >= (n + 1)) do 
  begin 
    print (x * x);
    set x := x + 1 
  end 
end 
