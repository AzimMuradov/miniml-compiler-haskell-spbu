#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>


// Compiler compliance checks

#ifndef __STDC_VERSION__
  #error "Not a C11 standard compliant compiler"
#endif

#if __STDC_VERSION__ < 201112L
  #error "Not a C11 standard compliant compiler"
#endif


// Standard primitive types

#define unit_t int64_t
#define boolean_t int64_t
#define integer_t int64_t


// Standard functions

boolean_t not(const boolean_t x) {
    return !x;
}

unit_t print_bool(const boolean_t x) {
    printf("%s\n", x ? "true" : "false");
    return 0;
}

unit_t print_int(const integer_t x) {
    printf("%" PRId64 "\n", x);
    return 0;
}
