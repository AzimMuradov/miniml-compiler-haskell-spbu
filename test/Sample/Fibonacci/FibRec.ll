declare external ccc i64 @not(i64)

declare external ccc i64 @print_bool(i64)

declare external ccc i64 @print_int(i64)

declare external ccc i64 @miniml_div(i64, i64)

declare external ccc i64 @miniml_fun_to_paf(i64, i64)

declare external ccc i64 @miniml_apply(i64, i64)

define external ccc i64 @fib.1(i64 %n.2_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = icmp slt i64 %n.2_0, 2
  %1 = zext i1 %0 to i64
  %2 = trunc i64 %1 to i1
  br i1 %2, label %if.then_0, label %if.else_0
if.then_0:
  store i64 %n.2_0, ptr %stack.ptr_0
  br label %if.end_0
if.else_0:
  %3 = sub i64 %n.2_0, 1
  %4 = ptrtoint ptr @fib.1 to i64
  %5 = call ccc i64 @miniml_fun_to_paf(i64 %4, i64 1)
  %6 = call ccc i64 @miniml_apply(i64 %5, i64 %3)
  %7 = sub i64 %n.2_0, 2
  %8 = ptrtoint ptr @fib.1 to i64
  %9 = call ccc i64 @miniml_fun_to_paf(i64 %8, i64 1)
  %10 = call ccc i64 @miniml_apply(i64 %9, i64 %7)
  %11 = add i64 %6, %10
  store i64 %11, ptr %stack.ptr_0
  br label %if.end_0
if.end_0:
  %12 = load i64, ptr %stack.ptr_0
  ret i64 %12
}

@simp.3 = global i64 0

define external ccc i64 @main() {
start:
  %0 = ptrtoint ptr @fib.1 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 1)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 10)
  %3 = ptrtoint ptr @print_int to i64
  %4 = call ccc i64 @miniml_fun_to_paf(i64 %3, i64 1)
  %5 = call ccc i64 @miniml_apply(i64 %4, i64 %2)
  store i64 %5, ptr @simp.3
  ret i64 0
}