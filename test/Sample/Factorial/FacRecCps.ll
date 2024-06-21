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

define external ccc i64 @ll.10(i64 %n.4_0, i64 %k.5_0, i64 %result.6_0) {
start:
  %0 = mul i64 %n.4_0, %result.6_0
  %1 = call ccc i64 @miniml_apply(i64 %k.5_0, i64 %0)
  ret i64 %1
}

define external ccc i64 @cps_factorial.3(i64 %n.4_0, i64 %k.5_0) {
start:
  %stack.ptr_0 = alloca i64
  %0 = icmp eq i64 %n.4_0, 0
  %1 = zext i1 %0 to i64
  %2 = trunc i64 %1 to i1
  br i1 %2, label %if.then_0, label %if.else_0
if.then_0:
  %3 = call ccc i64 @miniml_apply(i64 %k.5_0, i64 1)
  store i64 %3, ptr %stack.ptr_0
  br label %if.end_0
if.else_0:
  %4 = sub i64 %n.4_0, 1
  %5 = ptrtoint ptr @cps_factorial.3 to i64
  %6 = call ccc i64 @miniml_fun_to_paf(i64 %5, i64 2)
  %7 = call ccc i64 @miniml_apply(i64 %6, i64 %4)
  %8 = ptrtoint ptr @ll.10 to i64
  %9 = call ccc i64 @miniml_fun_to_paf(i64 %8, i64 3)
  %10 = call ccc i64 @miniml_apply(i64 %9, i64 %n.4_0)
  %11 = call ccc i64 @miniml_apply(i64 %10, i64 %k.5_0)
  %12 = call ccc i64 @miniml_apply(i64 %7, i64 %11)
  store i64 %12, ptr %stack.ptr_0
  br label %if.end_0
if.end_0:
  %13 = load i64, ptr %stack.ptr_0
  ret i64 %13
}

define external ccc i64 @factorial.8(i64 %n.7_0) {
start:
  %0 = ptrtoint ptr @cps_factorial.3 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 2)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 %n.7_0)
  %3 = ptrtoint ptr @id.2 to i64
  %4 = call ccc i64 @miniml_fun_to_paf(i64 %3, i64 1)
  %5 = call ccc i64 @miniml_apply(i64 %2, i64 %4)
  ret i64 %5
}

@simp.9 = global i64 0

define external ccc i64 @main() {
start:
  %0 = ptrtoint ptr @factorial.8 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 1)
  %2 = call ccc i64 @miniml_apply(i64 %1, i64 5)
  %3 = ptrtoint ptr @print_int to i64
  %4 = call ccc i64 @miniml_fun_to_paf(i64 %3, i64 1)
  %5 = call ccc i64 @miniml_apply(i64 %4, i64 %2)
  store i64 %5, ptr @simp.9
  ret i64 0
}