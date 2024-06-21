declare external ccc i64 @not(i64)

declare external ccc i64 @print_bool(i64)

declare external ccc i64 @print_int(i64)

declare external ccc i64 @miniml_div(i64, i64)

declare external ccc i64 @miniml_fun_to_paf(i64, i64)

declare external ccc i64 @miniml_apply(i64, i64)

define external ccc i64 @loop.2(i64 %i.3_0, i64 %a.4_0, i64 %b.5_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = icmp eq i64 %i.3_0, 0
  %1 = zext i1 %0 to i64
  %2 = trunc i64 %1 to i1
  br i1 %2, label %if.then_0, label %if.else_0
if.then_0:
  store i64 %a.4_0, ptr %stack.ptr_0
  br label %if.end_0
if.else_0:
  %3 = sub i64 %i.3_0, 1
  %4 = ptrtoint ptr @loop.2 to i64
  %5 = call ccc i64 @miniml_fun_to_paf(i64 %4, i64 3)
  %6 = call ccc i64 @miniml_apply(i64 %5, i64 %3)
  %7 = call ccc i64 @miniml_apply(i64 %6, i64 %b.5_0)
  %8 = add i64 %a.4_0, %b.5_0
  %9 = call ccc i64 @miniml_apply(i64 %7, i64 %8)
  store i64 %9, ptr %stack.ptr_0
  br label %if.end_0
if.end_0:
  %10 = load i64, ptr %stack.ptr_0
  ret i64 %10
}

define external ccc i64 @fib.6(i64 %n.1_0) {
start:
  %0 = ptrtoint ptr @loop.2 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 3)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 %n.1_0)
  %3 = call ccc i64 @miniml_apply(i64 %2, i64 0)
  %4 = call ccc i64 @miniml_apply(i64 %3, i64 1)
  ret i64 %4
}

@simp.7 = global i64 0

define external ccc i64 @main() {
start:
  %0 = ptrtoint ptr @fib.6 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 1)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 10)
  %3 = ptrtoint ptr @print_int to i64
  %4 = call ccc i64 @miniml_fun_to_paf(i64 %3, i64 1)
  %5 = call ccc i64 @miniml_apply(i64 %4, i64 %2)
  store i64 %5, ptr @simp.7
  ret i64 0
}