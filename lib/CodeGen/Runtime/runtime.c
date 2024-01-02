#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


// Compiler Compliance Checks

#ifndef __STDC_VERSION__
  #error "Not a C11 standard compliant compiler"
#endif

#if __STDC_VERSION__ < 201112L
  #error "Not a C11 standard compliant compiler"
#endif

// For Non-LLVM Compilers

#ifndef __has_builtin
  #define __has_builtin(a) 0
#endif


// The Standard Library

// Standard Primitive Types

#define unit_t int64_t
#define boolean_t int64_t
#define integer_t int64_t

// Standard Functions

boolean_t not(const boolean_t a) {
  return !a;
}

unit_t print_bool(const boolean_t a) {
  printf("%s\n", a ? "true" : "false");
  return 0;
}

unit_t print_int(const integer_t a) {
  printf("%" PRId64 "\n", a);
  return 0;
}


// Internal

static _Noreturn void miniml_throw_exception(const char *msg);

// UB-Free Arithmetic Operations

integer_t miniml_div(const integer_t a, const integer_t b) {
  if (b == 0) {
    miniml_throw_exception("Exception: Division_by_zero.");
  }
  if (a == INT64_MIN && b == -1) {
    return INT64_MIN;
  }
  return a / b;
}

// Exception Utils

static _Noreturn void miniml_throw_exception(const char *const msg) {
  fprintf(stderr, "%s\n", msg);
  exit(EXIT_FAILURE);
}
