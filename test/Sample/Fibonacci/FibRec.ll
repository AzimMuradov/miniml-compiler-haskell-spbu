; ModuleID = 'fibonacci'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


declare external ccc  i64 @miniml_div(i64, i64)    


declare external ccc  i64 @miniml_fun_to_paf(i64, i64)    


declare external ccc  i64 @miniml_apply(i64, i64)    


define external ccc  i64 @fib.1(i64  %n.2_0)    {
; <label>:0:
  %1 = alloca i64 
  %2 = icmp slt i64 %n.2_0, 2 
  %3 = zext i1 %2 to i64  
  %4 = trunc i64 %3 to i1  
  br i1 %4, label %if.then_0, label %if.else_0 
if.then_0:
  store  i64 %n.2_0, i64* %1 
  br label %if.end_0 
if.else_0:
  %anf.4_0 = ptrtoint i64 (i64)* @fib.1 to i64 
  %anf.4_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.4_0, i64  1)  
  %anf.4_2 = sub   i64 %n.2_0, 1 
  %anf.4_3 =  call ccc  i64  @miniml_apply(i64  %anf.4_1, i64  %anf.4_2)  
  %anf.5_0 = ptrtoint i64 (i64)* @fib.1 to i64 
  %anf.5_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.5_0, i64  1)  
  %anf.5_2 = sub   i64 %n.2_0, 2 
  %anf.5_3 =  call ccc  i64  @miniml_apply(i64  %anf.5_1, i64  %anf.5_2)  
  %5 = add   i64 %anf.4_3, %anf.5_3 
  store  i64 %5, i64* %1 
  br label %if.end_0 
if.end_0:
  %6 = load  i64, i64* %1 
  ret i64 %6 
}


@simp.3 =    global i64 0


define external ccc  i64 @main()    {
  %anf.6_0 = ptrtoint i64 (i64)* @fib.1 to i64 
  %anf.6_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.6_0, i64  1)  
  %anf.6_2 =  call ccc  i64  @miniml_apply(i64  %anf.6_1, i64  10)  
  %1 = ptrtoint i64 (i64)* @print_int to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %2, i64  %anf.6_2)  
  store  i64 %3, i64* @simp.3 
  ret i64 0 
}