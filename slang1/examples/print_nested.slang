
% should print 
% 
% 3
% 2 
% 1 

print 
  begin 
      set x := 1; 
      print 
        begin 
	  set y := 2;  	  
          print 
            begin 
   	     set z := 3;  	  
             z
            end; 
          y 
        end; 
       x
  end 


