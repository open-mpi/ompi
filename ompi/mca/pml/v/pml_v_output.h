/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_V_OUTPUT_H_HAS_BEEN_INCLUDED
#define PML_V_OUTPUT_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "pml_v.h"

#include <inttypes.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

int pml_v_output_init(char *output, int verbosity);
void pml_v_output_finalize(void);

/* Tricky stuff to define V_OUTPUT and V_OUTPUT_VERBOSE with variadic arguments
 */
#if   defined(ACCEPT_C99)
#   define V_OUTPUT(ARGS...) OPAL_OUTPUT((mca_pml_v.output, __VA_ARGS__))
#   define V_OUTPUT_VERBOSE(V, ARGS...) OPAL_OUTPUT_VERBOSE((V, mca_pml_v.output, __VA_ARGS__))
#elif defined(__GNUC__) && !defined(__STDC__)
#   define V_OUTPUT(ARGS...) OPAL_OUTPUT((mca_pml_v.output, ARGS))
#   define V_OUTPUT_VERBOSE(V, ARGS...) OPAL_OUTPUT_VERBOSE((V, mca_pml_v.output, ARGS))
#elif OMPI_ENABLE_DEBUG
    /* No variadic macros available... So sad */
static inline void V_OUTPUT(const char* fmt, ... ) {
    va_list list;
    char *str;
    int ret;
    va_start(list, fmt);
    ret = vasprintf(&str, fmt, list);
    assert(-1 != ret);
    opal_output(mca_pml_v.output, str);
    free(str);
    va_end(list);
}
static inline void V_OUTPUT_VERBOSE(int V, const char* fmt, ... ) {
    va_list list;
    char *str;
    int ret;
    va_start(list, fmt);
    ret = vasprintf(&str, fmt, list);
    assert(-1 != ret);
    opal_output_verbose(V, mca_pml_v.output, str);
    free(str);
    va_end(list);
}
#else /* !DEBUG */
   /* Some compilers complain if we have ... and no corresponding va_start() */
static inline void V_OUTPUT(const char* fmt, ... ) {
#if defined(__PGI)
    va_list list;
    va_start(list, fmt);
    va_end(list);
#endif
}
static inline void V_OUTPUT_VERBOSE(int V, const char* fmt, ... ) {
#if defined(__PGI)
    va_list list;
    va_start(list, fmt);
    va_end(list);
#endif
}
#endif /* DEBUG */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* PML_V_OUTPUT_H_HAS_BEEN_INCLUDED */
