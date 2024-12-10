/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"
#ifdef MPL_HAVE_BACKTRACE
#include <execinfo.h>

#define MAX_TRACE_DEPTH 32

void MPL_backtrace_show(FILE * output)
{
    void *trace[MAX_TRACE_DEPTH];
    _mpl_backtrace_size_t frames;

    frames = backtrace(trace, MAX_TRACE_DEPTH);
    char **strs = backtrace_symbols(trace, frames);
    for (_mpl_backtrace_size_t i = 0; i < frames; i++)
        fprintf(output, "%s\n", strs[i]);

    MPL_external_free(strs);
}
#else
void MPL_backtrace_show(FILE * output)
{
    fprintf(output, "No backtrace info available\n");
}
#endif
