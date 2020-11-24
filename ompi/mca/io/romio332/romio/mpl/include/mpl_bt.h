/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2015 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */


#ifndef MPL_BT_H_INCLUDED
#define MPL_BT_H_INCLUDED

#include "mplconfig.h"
#if defined(__cplusplus)
extern "C" {
#endif

#if !MPL_HAVE_DECL_BACKTRACE_CREATE_STATE
    struct backtrace_state;
    typedef void (*backtrace_error_callback) (void *data, const char *msg, int errnum);

    extern struct backtrace_state *backtrace_create_state(const char *filename, int threaded,
                                                          backtrace_error_callback error_callback,
                                                          void *data);
#endif
#if !MPL_HAVE_DECL_BACKTRACE_PRINT
    extern void backtrace_print(struct backtrace_state *state, int skip, FILE *);
#endif

#define MPL_BACKTRACE_BUFFER_LEN 1024
#define MPL_MAX_TRACE_DEPTH 32
    void MPL_backtrace_show(FILE * output);

#if defined(__cplusplus)

}
#endif
#endif                          /* MPL_BT_H_INCLUDED */
