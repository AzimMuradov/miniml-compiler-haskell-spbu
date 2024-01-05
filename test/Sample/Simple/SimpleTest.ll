; ModuleID = 'simpleTest'


 


declare external ccc  i64 @not(i64)    


declare external ccc  i64 @print_bool(i64)    


declare external ccc  i64 @print_int(i64)    


declare external ccc  i64 @miniml_div(i64, i64)    


declare external ccc  i64 @miniml_fun_to_paf(i64, i64)    


declare external ccc  i64 @miniml_apply(i64, i64)    


define external ccc  i64 @id.2(i64  %x.1_0)    {
  ret i64 %x.1_0 
}


define external ccc  i64 @k.4(i64  %x.3_0)    {
  %1 =  call ccc  i64  @miniml_apply(i64  %x.3_0, i64  42)  
  ret i64 %1 
}


@simp.5 =    global i64 0


define external ccc  i64 @main()    {
  %anf.6_0 = ptrtoint i64 (i64)* @k.4 to i64 
  %anf.6_1 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.6_0, i64  1)  
  %anf.6_2 = ptrtoint i64 (i64)* @id.2 to i64 
  %anf.6_3 =  call ccc  i64  @miniml_fun_to_paf(i64  %anf.6_2, i64  1)  
  %anf.6_4 =  call ccc  i64  @miniml_apply(i64  %anf.6_1, i64  %anf.6_3)  
  %1 = ptrtoint i64 (i64)* @print_int to i64 
  %2 =  call ccc  i64  @miniml_fun_to_paf(i64  %1, i64  1)  
  %3 =  call ccc  i64  @miniml_apply(i64  %2, i64  %anf.6_4)  
  store  i64 %3, i64* @simp.5 
  ret i64 0 
}