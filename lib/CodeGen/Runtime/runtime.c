#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// Compiler Compliance Checks

#ifndef __STDC_VERSION__
  #error "Not a C11 standard compliant compiler"
#endif

#if __STDC_VERSION__ < 201112L
  #error "Not a C11 standard compliant compiler"
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

// Partial Application

#define any_t int64_t
#define paf_ptr_t int64_t

struct MiniMlPartAppFun {
  void* fun;
  size_t params_cnt;
  any_t* args;
  size_t args_cnt;
};

paf_ptr_t miniml_fun_to_paf(any_t fun, const integer_t params_cnt) {
  struct MiniMlPartAppFun* paf = malloc(sizeof(struct MiniMlPartAppFun));
  *paf = (struct MiniMlPartAppFun){
    .fun = (struct MiniMlPartAppFun*) fun,
    .params_cnt = (size_t) params_cnt,
    .args = NULL,
    .args_cnt = 0,
  };
  return (paf_ptr_t) paf;
}

static any_t miniml_apply_stored_args_1(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_2(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_3(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_4(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_5(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_6(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_7(const struct MiniMlPartAppFun paf, const any_t arg);
static any_t miniml_apply_stored_args_8(const struct MiniMlPartAppFun paf, const any_t arg);

any_t miniml_apply(paf_ptr_t _paf, const any_t arg) {
  struct MiniMlPartAppFun* paf = (struct MiniMlPartAppFun*) _paf;

  if (paf->params_cnt == paf->args_cnt + 1) {
    switch (paf->params_cnt) {
      case 1: return miniml_apply_stored_args_1(*paf, arg);
      case 2: return miniml_apply_stored_args_2(*paf, arg);
      case 3: return miniml_apply_stored_args_3(*paf, arg);
      case 4: return miniml_apply_stored_args_4(*paf, arg);
      case 5: return miniml_apply_stored_args_5(*paf, arg);
      case 6: return miniml_apply_stored_args_6(*paf, arg);
      case 7: return miniml_apply_stored_args_7(*paf, arg);
      case 8: return miniml_apply_stored_args_8(*paf, arg);
      default: miniml_throw_exception("Too many arguments!");
    }
  }

  const size_t args_cnt = paf->args_cnt;
  const size_t new_args_cnt = args_cnt + 1;

  struct MiniMlPartAppFun* new_paf = malloc(sizeof(struct MiniMlPartAppFun));
  *new_paf = (struct MiniMlPartAppFun){
    .fun = paf->fun,
    .params_cnt = paf->params_cnt,
    .args = malloc(new_args_cnt * sizeof(any_t)),
    .args_cnt = new_args_cnt,
  };
  if (args_cnt > 0) {
    memcpy(new_paf->args, paf->args, args_cnt * sizeof(any_t));
  }
  new_paf->args[args_cnt] = arg;
  return (any_t) new_paf;
}

static any_t miniml_apply_stored_args_1(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t) = paf.fun;
  return fun(arg);
}

static any_t miniml_apply_stored_args_2(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t) = paf.fun;
  return fun(paf.args[0], arg);
}

static any_t miniml_apply_stored_args_3(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t,any_t) = paf.fun;
  return fun(paf.args[0], paf.args[1], arg);
}

static any_t miniml_apply_stored_args_4(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t,any_t,any_t) = paf.fun;
  return fun(
    paf.args[0], paf.args[1], paf.args[2], arg
  );
}

static any_t miniml_apply_stored_args_5(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t,any_t,any_t,any_t) = paf.fun;
  return fun(
    paf.args[0], paf.args[1], paf.args[2], paf.args[3],
    arg
  );
}

static any_t miniml_apply_stored_args_6(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t,any_t,any_t,any_t,any_t) = paf.fun;
  return fun(
    paf.args[0], paf.args[1], paf.args[2], paf.args[3],
    paf.args[4], arg
  );
}

static any_t miniml_apply_stored_args_7(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t,any_t,any_t,any_t,any_t,any_t) = paf.fun;
  return fun(
    paf.args[0], paf.args[1], paf.args[2], paf.args[3],
    paf.args[4], paf.args[5], arg
  );
}

static any_t miniml_apply_stored_args_8(const struct MiniMlPartAppFun paf, const any_t arg) {
  any_t (*fun)(any_t,any_t,any_t,any_t,any_t,any_t,any_t,any_t) = paf.fun;
  return fun(
    paf.args[0], paf.args[1], paf.args[2], paf.args[3],
    paf.args[4], paf.args[5], paf.args[6], arg
  );
}

// Exception Utils

static _Noreturn void miniml_throw_exception(const char *const msg) {
  fprintf(stderr, "%s\n", msg);
  exit(EXIT_FAILURE);
}
