% a good program to find bugs in the compiler!
% should print "-40" 
  begin 
      set x := 10 ; 
      set x := (begin set x := 4 * x ; x end) - (begin set x := 2 * x ; x end);  
      print x
  end 


