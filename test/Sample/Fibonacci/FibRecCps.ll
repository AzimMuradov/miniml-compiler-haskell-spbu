; ModuleID = 'fibonacci'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


declare external ccc  i64 @miniml_div(i64, i64)    


declare external ccc  i64 @miniml_fun_to_paf(i64, i64)    


declare external ccc  i64 @miniml_apply(i64, i64)    


define external ccc  i64 @id.2(i64  %x.1_0)    {
  ret i64 %x.1_0 
}


define external ccc  i64 @ll.12(i64  %k.6_0, i64  %a.7_0, i64  %b.8_0)    {
  %1 = add   i64 %a.7_0, %b.8_0 
  %2 =  call ccc  i64  @miniml_apply(i64  %k.6_0, i64  %1)  
  ret i64 %2 
}


define external ccc  i64 @ll.11(i64  %fib_cps.4_0, i64  %n.5_0, i64  %k.6_0, i64  %a.7_0)    {
  %anf.13_0 = sub   i64 %n.5_0, 2 
  %anf.13_1 =  call ccc  i64  @miniml_apply(i64  %fib_cps.4_0, i64  %anf.13_0)  
  %anf.14_0 = ptrtoint i64 (i64, i64, i64)* @ll.12 to i64 
  %anf.14_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.14_0, i64  3)  
  %anf.14_2 =  call ccc  i64  @miniml_apply(i64  %anf.14_1, i64  %k.6_0)  
  %anf.15_0 =  call ccc  i64  @miniml_apply(i64  %anf.14_2, i64  %a.7_0)  
  %1 =  call ccc  i64  @miniml_apply(i64  %anf.13_1, i64  %anf.15_0)  
  ret i64 %1 
}


define external ccc  i64 @fib_cps.4(i64  %n.5_0, i64  %k.6_0)    {
; <label>:0:
  %1 = alloca i64 
  %2 = icmp slt i64 %n.5_0, 3 
  %3 = zext i1 %2 to i64  
  %4 = trunc i64 %3 to i1  
  br i1 %4, label %if.then_0, label %if.else_0 
if.then_0:
  %5 =  call ccc  i64  @miniml_apply(i64  %k.6_0, i64  1)  
  store  i64 %5, i64* %1 
  br label %if.end_0 
if.else_0:
  %anf.16_0 = ptrtoint i64 (i64, i64)* @fib_cps.4 to i64 
  %anf.16_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.16_0, i64  2)  
  %anf.16_2 = sub   i64 %n.5_0, 1 
  %anf.16_3 =  call ccc  i64  @miniml_apply(i64  %anf.16_1, i64  %anf.16_2)  
  %anf.17_0 = ptrtoint i64 (i64, i64, i64, i64)* @ll.11 to i64 
  %anf.17_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.17_0, i64  4)  
  %anf.17_2 = ptrtoint i64 (i64, i64)* @fib_cps.4 to i64 
  %anf.17_3 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.17_2, i64  2)  
  %anf.17_4 =  call ccc  i64  @miniml_apply(i64  %anf.17_1, i64  %anf.17_3)  
  %anf.18_0 =  call ccc  i64  @miniml_apply(i64  %anf.17_4, i64  %n.5_0)  
  %anf.19_0 =  call ccc  i64  @miniml_apply(i64  %anf.18_0, i64  %k.6_0)  
  %6 =  call ccc  i64  @miniml_apply(i64  %anf.16_3, i64  %anf.19_0)  
  store  i64 %6, i64* %1 
  br label %if.end_0 
if.end_0:
  %7 = load  i64, i64* %1 
  ret i64 %7 
}


define external ccc  i64 @fib.9(i64  %n.3_0)    {
  %anf.20_0 = ptrtoint i64 (i64, i64)* @fib_cps.4 to i64 
  %anf.20_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.20_0, i64  2)  
  %anf.20_2 =  call ccc  i64  @miniml_apply(i64  %anf.20_1, i64  %n.3_0)  
  %1 = ptrtoint i64 (i64)* @id.2 to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %anf.20_2, i64  %2)  
  ret i64 %3 
}


@simp.10 =    global i64 0


define external ccc  i64 @main()    {
  %anf.21_0 = ptrtoint i64 (i64)* @fib.9 to i64 
  %anf.21_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.21_0, i64  1)  
  %anf.21_2 =  call ccc  i64  @miniml_apply(i64  %anf.21_1, i64  10)  
  %1 = ptrtoint i64 (i64)* @print_int to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %2, i64  %anf.21_2)  
  store  i64 %3, i64* @simp.10 
  ret i64 0 
}