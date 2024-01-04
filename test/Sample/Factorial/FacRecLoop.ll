; ModuleID = 'factorial'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


declare external ccc  i64 @miniml_div(i64, i64)    


declare external ccc  i64 @miniml_fun_to_paf(i64, i64)    


declare external ccc  i64 @miniml_apply(i64, i64)    


define external ccc  i64 @loop.2(i64  %n.1_0, i64  %i.3_0, i64  %accum.4_0)    {
; <label>:0:
  %1 = alloca i64 
  %2 = icmp sgt i64 %i.3_0, %n.1_0 
  %3 = zext i1 %2 to i64  
  %4 = trunc i64 %3 to i1  
  br i1 %4, label %if.then_0, label %if.else_0 
if.then_0:
  store  i64 %accum.4_0, i64* %1 
  br label %if.end_0 
if.else_0:
  %anf.7_0 = ptrtoint i64 (i64, i64, i64)* @loop.2 to i64 
  %anf.7_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.7_0, i64  3)  
  %anf.7_2 =  call ccc  i64  @miniml_apply(i64  %anf.7_1, i64  %n.1_0)  
  %anf.8_0 = add   i64 %i.3_0, 1 
  %anf.8_1 =  call ccc  i64  @miniml_apply(i64  %anf.7_2, i64  %anf.8_0)  
  %5 = mul   i64 %accum.4_0, %i.3_0 
  %6 =  call ccc  i64  @miniml_apply(i64  %anf.8_1, i64  %5)  
  store  i64 %6, i64* %1 
  br label %if.end_0 
if.end_0:
  %7 = load  i64, i64* %1 
  ret i64 %7 
}


define external ccc  i64 @factorial.5(i64  %n.1_0)    {
  %anf.9_0 = ptrtoint i64 (i64, i64, i64)* @loop.2 to i64 
  %anf.9_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.9_0, i64  3)  
  %anf.9_2 =  call ccc  i64  @miniml_apply(i64  %anf.9_1, i64  %n.1_0)  
  %anf.10_0 =  call ccc  i64  @miniml_apply(i64  %anf.9_2, i64  1)  
  %1 =  call ccc  i64  @miniml_apply(i64  %anf.10_0, i64  1)  
  ret i64 %1 
}


@simp.6 =    global i64 0


define external ccc  i64 @main()    {
  %anf.11_0 = ptrtoint i64 (i64)* @factorial.5 to i64 
  %anf.11_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.11_0, i64  1)  
  %anf.11_2 =  call ccc  i64  @miniml_apply(i64  %anf.11_1, i64  5)  
  %1 = ptrtoint i64 (i64)* @print_int to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %2, i64  %anf.11_2)  
  store  i64 %3, i64* @simp.6 
  ret i64 0 
}