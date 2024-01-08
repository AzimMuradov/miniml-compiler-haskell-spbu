; ModuleID = 'unnamed'


 


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
  %anf.11_0 = mul   i64 %n.4_0, %result.6_0 
  %1 =  call ccc  i64  @miniml_apply(i64  %k.5_0, i64  %anf.11_0)  
  ret i64 %1 
}


define external ccc  i64 @cps_factorial.3(i64  %n.4_0, i64  %k.5_0)    {
; <label>:0:
  %anf.12_0 = icmp eq i64 %n.4_0, 0 
  %anf.12_1 = zext i1 %anf.12_0 to i64  
  %1 = alloca i64 
  %2 = trunc i64 %anf.12_1 to i1  
  br i1 %2, label %if.then_0, label %if.else_0 
if.then_0:
  %3 =  call ccc  i64  @miniml_apply(i64  %k.5_0, i64  1)  
  store  i64 %3, i64* %1 
  br label %if.end_0 
if.else_0:
  %anf.13_0 = sub   i64 %n.4_0, 1 
  %anf.14_0 = ptrtoint i64 (i64, i64)* @cps_factorial.3 to i64 
  %anf.14_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.14_0, i64  2)  
  %anf.14_2 =  call ccc  i64  @miniml_apply(i64  %anf.14_1, i64  %anf.13_0)  
  %anf.15_0 = ptrtoint i64 (i64, i64, i64)* @ll.10 to i64 
  %anf.15_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.15_0, i64  3)  
  %anf.15_2 =  call ccc  i64  @miniml_apply(i64  %anf.15_1, i64  %n.4_0)  
  %anf.16_0 =  call ccc  i64  @miniml_apply(i64  %anf.15_2, i64  %k.5_0)  
  %4 =  call ccc  i64  @miniml_apply(i64  %anf.14_2, i64  %anf.16_0)  
  store  i64 %4, i64* %1 
  br label %if.end_0 
if.end_0:
  %5 = load  i64, i64* %1 
  ret i64 %5 
}


define external ccc  i64 @factorial.8(i64  %n.7_0)    {
  %anf.17_0 = ptrtoint i64 (i64, i64)* @cps_factorial.3 to i64 
  %anf.17_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.17_0, i64  2)  
  %anf.17_2 =  call ccc  i64  @miniml_apply(i64  %anf.17_1, i64  %n.7_0)  
  %1 = ptrtoint i64 (i64)* @id.2 to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %anf.17_2, i64  %2)  
  ret i64 %3 
}


@simp.9 =    global i64 0


define external ccc  i64 @main()    {
  %anf.18_0 = ptrtoint i64 (i64)* @factorial.8 to i64 
  %anf.18_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.18_0, i64  1)  
  %anf.18_2 =  call ccc  i64  @miniml_apply(i64  %anf.18_1, i64  5)  
  %1 = ptrtoint i64 (i64)* @print_int to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %2, i64  %anf.18_2)  
  store  i64 %3, i64* @simp.9 
  ret i64 0 
}