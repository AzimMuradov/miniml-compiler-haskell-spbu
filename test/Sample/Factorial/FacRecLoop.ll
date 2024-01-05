; ModuleID = 'factorial'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


declare external ccc  i64 @miniml_div(i64, i64)    


declare external ccc  i64 @miniml_fun_to_paf(i64, i64)    


declare external ccc  i64 @miniml_apply(i64, i64)    


define external ccc  i64 @loop.2(i64  %n.1_0, i64  %i.3_0, i64  %accum.4_0)    {
; <label>:0:
  %anf.7_0 = icmp sgt i64 %i.3_0, %n.1_0 
  %anf.7_1 = zext i1 %anf.7_0 to i64  
  %1 = alloca i64 
  %2 = trunc i64 %anf.7_1 to i1  
  br i1 %2, label %if.then_0, label %if.else_0 
if.then_0:
  store  i64 %accum.4_0, i64* %1 
  br label %if.end_0 
if.else_0:
  %anf.8_0 = ptrtoint i64 (i64, i64, i64)* @loop.2 to i64 
  %anf.8_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.8_0, i64  3)  
  %anf.8_2 =  call ccc  i64  @miniml_apply(i64  %anf.8_1, i64  %n.1_0)  
  %anf.9_0 = add   i64 %i.3_0, 1 
  %anf.10_0 =  call ccc  i64  @miniml_apply(i64  %anf.8_2, i64  %anf.9_0)  
  %anf.11_0 = mul   i64 %accum.4_0, %i.3_0 
  %3 =  call ccc  i64  @miniml_apply(i64  %anf.10_0, i64  %anf.11_0)  
  store  i64 %3, i64* %1 
  br label %if.end_0 
if.end_0:
  %4 = load  i64, i64* %1 
  ret i64 %4 
}


define external ccc  i64 @factorial.5(i64  %n.1_0)    {
  %anf.12_0 = ptrtoint i64 (i64, i64, i64)* @loop.2 to i64 
  %anf.12_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.12_0, i64  3)  
  %anf.12_2 =  call ccc  i64  @miniml_apply(i64  %anf.12_1, i64  %n.1_0)  
  %anf.13_0 =  call ccc  i64  @miniml_apply(i64  %anf.12_2, i64  1)  
  %1 =  call ccc  i64  @miniml_apply(i64  %anf.13_0, i64  1)  
  ret i64 %1 
}


@simp.6 =    global i64 0


define external ccc  i64 @main()    {
  %anf.14_0 = ptrtoint i64 (i64)* @factorial.5 to i64 
  %anf.14_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.14_0, i64  1)  
  %anf.14_2 =  call ccc  i64  @miniml_apply(i64  %anf.14_1, i64  5)  
  %1 = ptrtoint i64 (i64)* @print_int to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %2, i64  %anf.14_2)  
  store  i64 %3, i64* @simp.6 
  ret i64 0 
}