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

define external ccc i64 @k.4(i64 %x.3_0) {
start:
  %0 = call ccc i64 @miniml_apply(i64 %x.3_0, i64 42)
  ret i64 %0
}

@simp.5 = global i64 0

define external ccc i64 @main() {
start:
  %0 = ptrtoint ptr @k.4 to i64
  %1 = call ccc i64 @miniml_fun_to_paf(i64 %0, i64 1)
  %2 = ptrtoint ptr @id.2 to i64
  %3 = call ccc i64 @miniml_fun_to_paf(i64 %2, i64 1)
  %4 = call ccc i64 @miniml_apply(i64 %1, i64 %3)
  %5 = ptrtoint ptr @print_int to i64
  %6 = call ccc i64 @miniml_fun_to_paf(i64 %5, i64 1)
  %7 = call ccc i64 @miniml_apply(i64 %6, i64 %4)
  store i64 %7, ptr @simp.5
  ret i64 0
}