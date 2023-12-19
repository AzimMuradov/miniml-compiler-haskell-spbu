; ModuleID = 'factorial'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


define external ccc  i64 @"factorial'0"(i64  %"n'1_0")    {
; <label>:0:
  %1 = alloca i64 
  %2 = icmp sle i64 %"n'1_0", 0 
  %3 = zext i1 %2 to i64  
  %4 = trunc i64 %3 to i64  
  br i64 %4, label %if.then_0, label %if.else_0 
if.then_0:
  store  i64 1, i64* %1 
  br label %if.end_0 
if.else_0:
  %"anf'2_0" = sub   i64 %"n'1_0", 1 
  %"anf'2_1" =  call ccc  i64  @"factorial'0"(i64  %"anf'2_0")  
  %5 = mul   i64 %"n'1_0", %"anf'2_1" 
  store  i64 %5, i64* %1 
  br label %if.end_0 
if.end_0:
  %6 = load  i64, i64* %1 
  ret i64 %6 
}


define external ccc  i64 @main()    {
  ret i64 0 
}