#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>


// Compiler compliance checks

#ifndef __STDC_VERSION__
#  error "Not a C11 standard compliant compiler"
#endif

#if __STDC_VERSION__ < 201112L
#  error "Not a C11 standard compliant compiler"
#endif


// Standard primitive types

#if __STDC_VERSION__ >= 202311L
#  define boolean_t bool
#else
#  define boolean_t _Bool
#endif

#define integer_t int64_t


// Standard printing functions

integer_t print_bool(boolean_t x) {
    printf("%s\n", x ? "true" : "false");
    return 0;
}

integer_t print_int(integer_t x) {
    printf("%" PRId64 "\n", x);
    return 0;
}


// Standard boolean functions

boolean_t not(boolean_t x) {
    return !x;
}
