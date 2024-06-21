declare external ccc i64 @not(i64)

declare external ccc i64 @print_bool(i64)

declare external ccc i64 @print_int(i64)

declare external ccc i64 @miniml_div(i64, i64)

declare external ccc i64 @miniml_fun_to_paf(i64, i64)

declare external ccc i64 @miniml_apply(i64, i64)

define external ccc i64 @id.2(i64 %x.1_0) {
start:
  ret i64 %x.1_0
}

define external ccc i64 @ll.12(i64 %k.6_0, i64 %a.7_0, i64 %b.8_0) {
start:
  %0 = add i64 %a.7_0, %b.8_0
  %1 = call ccc i64 @miniml_apply(i64 %k.6_0, i64 %0)
  ret i64 %1
}

define external ccc i64 @ll.11(i64 %fib_cps.4_0, i64 %n.5_0, i64 %k.6_0, i64 %a.7_0) {
start:
  %0 = sub i64 %n.5_0, 2
  %1 = call ccc i64 @miniml_apply(i64 %fib_cps.4_0, i64 %0)
  %2 = ptrtoint ptr @ll.12 to i64
  %3 = call ccc i64 @miniml_fun_to_paf(i64 %2, i64 3)
  %4 = call ccc i64 @miniml_apply(i64 %3, i64 %k.6_0)
  %5 = call ccc i64 @miniml_apply(i64 %4, i64 %a.7_0)
  %6 = call ccc i64 @miniml_apply(i64 %1, i64 %5)
  ret i64 %6
}

define external ccc i64 @fib_cps.4(i64 %n.5_0, i64 %k.6_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = icmp slt i64 %n.5_0, 3
  %1 = zext i1 %0 to i64
  %2 = trunc i64 %1 to i1
  br i1 %2, label %if.then_0, label %if.else_0
if.then_0:
  %3 = call ccc i64 @miniml_apply(i64 %k.6_0, i64 1)
  store i64 %3, ptr %stack.ptr_0
  br label %if.end_0
if.else_0:
  %4 = sub i64 %n.5_0, 1
  %5 = ptrtoint ptr @fib_cps.4 to i64
  %6 = call ccc i64 @miniml_fun_to_paf(i64 %5, i64 2)
  %7 = call ccc i64 @miniml_apply(i64 %6, i64 %4)
  %8 = ptrtoint ptr @ll.11 to i64
  %9 = call ccc i64 @miniml_fun_to_paf(i64 %8, i64 4)
  %10 = ptrtoint ptr @fib_cps.4 to i64
  %11 = call ccc i64 @miniml_fun_to_paf(i64 %10, i64 2)
  %12 = call ccc i64 @miniml_apply(i64 %9, i64 %11)
  %13 = call ccc i64 @miniml_apply(i64 %12, i64 %n.5_0)
  %14 = call ccc i64 @miniml_apply(i64 %13, i64 %k.6_0)
  %15 = call ccc i64 @miniml_apply(i64 %7, i64 %14)
  store i64 %15, ptr %stack.ptr_0
  br label %if.end_0
if.end_0:
  %16 = load i64, ptr %stack.ptr_0
  ret i64 %16
}

define external ccc i64 @fib.9(i64 %n.3_0) {
start:
  %0 = ptrtoint ptr @fib_cps.4 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 2)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 %n.3_0)
  %3 = ptrtoint ptr @id.2 to i64
  %4 = call ccc i64 @miniml_fun_to_paf(i64 %3, i64 1)
  %5 = call ccc i64 @miniml_apply(i64 %2, i64 %4)
  ret i64 %5
}

@simp.10 = global i64 0

define external ccc i64 @main() {
start:
  %0 = ptrtoint ptr @fib.9 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 1)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 10)
  %3 = ptrtoint ptr @print_int to i64
  %4 = call ccc i64 @miniml_fun_to_paf(i64 %3, i64 1)
  %5 = call ccc i64 @miniml_apply(i64 %4, i64 %2)
  store i64 %5, ptr @simp.10
  ret i64 0
}