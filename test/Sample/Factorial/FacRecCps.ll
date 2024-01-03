; ModuleID = 'factorial'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


declare external ccc  i64 @miniml_div(i64, i64)    


declare external ccc  i64 @miniml_fun_to_paf(i64, i64)    


declare external ccc  i64 @miniml_apply(i64, i64)    


define external ccc  i64 @id.2(i64  %x.1_0)    {
  ret i64 %x.1_0 
}


define external ccc  i64 @ll.10(i64  %n.4_0, i64  %k.5_0, i64  %result.6_0)    {
  %1 = mul   i64 %n.4_0, %result.6_0 
  %2 =  call ccc  i64  @miniml_apply(i64  %k.5_0, i64  %1)  
  ret i64 %2 
}


define external ccc  i64 @cps_factorial.3(i64  %n.4_0, i64  %k.5_0)    {
; <label>:0:
  %1 = alloca i64 
  %2 = icmp eq i64 %n.4_0, 0 
  %3 = zext i1 %2 to i64  
  %4 = trunc i64 %3 to i1  
  br i1 %4, label %if.then_0, label %if.else_0 
if.then_0:
  %5 =  call ccc  i64  @miniml_apply(i64  %k.5_0, i64  1)  
  store  i64 %5, i64* %1 
  br label %if.end_0 
if.else_0:
  %anf.11_0 =  call ccc  i64  @miniml_fun_to_paf(i64 (i64, i64)*  @cps_factorial.3, i64  2)  
  %anf.11_1 = sub   i64 %n.4_0, 1 
  %anf.11_2 =  call ccc  i64  @miniml_apply(i64  %anf.11_0, i64  %anf.11_1)  
  %anf.12_0 =  call ccc  i64  @miniml_fun_to_paf(i64 (i64, i64, i64)*  @ll.10, i64  3)  
  %anf.12_1 =  call ccc  i64  @miniml_apply(i64  %anf.12_0, i64  %n.4_0)  
  %anf.13_0 =  call ccc  i64  @miniml_apply(i64  %anf.12_1, i64  %k.5_0)  
  %6 =  call ccc  i64  @miniml_apply(i64  %anf.11_2, i64  %anf.13_0)  
  store  i64 %6, i64* %1 
  br label %if.end_0 
if.end_0:
  %7 = load  i64, i64* %1 
  ret i64 %7 
}


define external ccc  i64 @factorial.8(i64  %n.7_0)    {
  %anf.14_0 =  call ccc  i64  @miniml_fun_to_paf(i64 (i64, i64)*  @cps_factorial.3, i64  2)  
  %anf.14_1 =  call ccc  i64  @miniml_apply(i64  %anf.14_0, i64  %n.7_0)  
  %1 =  call ccc  i64  @miniml_fun_to_paf(i64 (i64)*  @id.2, i64  1)  
  %2 =  call ccc  i64  @miniml_apply(i64  %anf.14_1, i64  %1)  
  ret i64 %2 
}


@simp.9 =    global i64 0


define external ccc  i64 @main()    {
  %anf.15_0 =  call ccc  i64  @miniml_fun_to_paf(i64 (i64)*  @factorial.8, i64  1)  
  %anf.15_1 =  call ccc  i64  @miniml_apply(i64  %anf.15_0, i64  5)  
  %1 =  call ccc  i64  @miniml_fun_to_paf(i64 (i64)*  @print_int, i64  1)  
  %2 =  call ccc  i64  @miniml_apply(i64  %1, i64  %anf.15_1)  
  store  i64 %2, i64* @simp.9 
  ret i64 0 
}