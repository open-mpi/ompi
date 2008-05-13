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

#include "orte/util/output.h"
#include "opal_stdint.h"
#include <stdio.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OMPI_DECLSPEC extern int pml_v_output;    

int pml_v_output_open(char *output, int verbosity);
void pml_v_output_close(void);

static inline void V_OUTPUT_ERR(const char *fmt, ... ) __opal_attribute_format__(__printf__, 1, 2);
static inline void V_OUTPUT_ERR(const char *fmt, ... )
{
    va_list list;
    char *str;
    int ret;
    va_start(list, fmt);
    ret = vasprintf(&str, fmt, list);
    assert(-1 != ret);
    orte_output(0, str);
    free(str);
    va_end(list);    
}

/* Tricky stuff to define V_OUTPUT and V_OUTPUT_VERBOSE with variadic arguments
 */
#if   defined(ACCEPT_C99)
#   define V_OUTPUT(ARGS...)                                                   \
        ORTE_OUTPUT((pml_v_output, __VA_ARGS__))
#   define V_OUTPUT_VERBOSE(V, ARGS...)                                        \
        ORTE_OUTPUT_VERBOSE((V, pml_v_output, __VA_ARGS__))

#elif defined(__GNUC__) && !defined(__STDC__)
#   define V_OUTPUT(ARGS...)                                                   \
        ORTE_OUTPUT((pml_v_output, ARGS))
#   define V_OUTPUT_VERBOSE(V, ARGS...)                                        \
        ORTE_OUTPUT_VERBOSE((V, pml_v_output, ARGS))
            
#elif OMPI_ENABLE_DEBUG
    /* No variadic macros available... So sad */
static inline void V_OUTPUT(const char* fmt, ... ) __opal_attribute_format__(__printf__, 1, 2);
static inline void V_OUTPUT(const char* fmt, ... )
{
    va_list list;
    char *str;
    int ret;
    va_start(list, fmt);
    ret = vasprintf(&str, fmt, list);
    assert(-1 != ret);
    orte_output(pml_v_output, str);
    free(str);
    va_end(list);
}
static inline void V_OUTPUT_VERBOSE(int V, const char* fmt, ... ) __opal_attribute_format__(__printf__, 2, 3);
static inline void V_OUTPUT_VERBOSE(int V, const char* fmt, ... ) {
    va_list list;
    char *str;
    int ret;
    va_start(list, fmt);
    ret = vasprintf(&str, fmt, list);
    assert(-1 != ret);
    orte_output_verbose(V, pml_v_output, str);
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
