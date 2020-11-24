/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2015 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"
#ifdef MPL_HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#ifdef MPL_HAVE_LIBBACKTRACE
#include <backtrace.h>
#endif

#ifdef MPL_HAVE_LIBUNWIND
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#endif


/* freebsd and linux have slightly different backtrace routines.
 * solaris uses something totally different: a 'walkcontext' routine
 * which takes a function pointer.  solaris 'walkcontext' is simliar to
 * libbacktrace, shipped with gcc-4.8 and newer.  both share features
 * with libunwind.
 *
 * For the case of "display the call stack to this point" the various
 * approaches share a common pattern:
 * - initialize the library
 * - get the stack
 * - decode the stack
 *
 * but for now we'll simply dispatch to one of several appraoches
 * depending on what configure found
 *
 */

#ifdef MPL_HAVE_LIBBACKTRACE

static inline void backtrace_libback(FILE * output)
{
    struct backtrace_state *btstate;
    btstate = backtrace_create_state(NULL, 1, NULL, NULL);
    backtrace_print(btstate, 0, output);
}

/* we need not only the symbols but the header file too (for the cursor and
 * context), so tighten up when we take the libunwind path.  Thanks
 * Siegmar.Gross@informatik.hs-fulda.de for the bug report about systems with
 * libunwind libraries but no libunwind development headers */
#elif defined MPL_HAVE_LIBUNWIND
static inline void backtrace_libunwind(FILE * output)
{
    unw_cursor_t cursor;
    unw_context_t uc;
    unw_word_t ip, offset;
    int ret, chars = 0;
    char buffer[MPL_BACKTRACE_BUFFER_LEN];
    char backtrace_buffer[MPL_BACKTRACE_BUFFER_LEN];

    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    while (unw_step(&cursor) > 0) {
        unw_get_reg(&cursor, UNW_REG_IP, &ip);
        unw_get_proc_name(&cursor, buffer, MPL_BACKTRACE_BUFFER_LEN, &offset);
        ret = MPL_snprintf(backtrace_buffer + chars,
                           MPL_BACKTRACE_BUFFER_LEN - chars,
                           "0x%lx %s() + 0x%lx\n", (long) ip, buffer, (long) offset);
        if (ret + chars >= MPL_BACKTRACE_BUFFER_LEN) {
            /* the extra new line will be more readable than a merely
             * truncated string */
            backtrace_buffer[MPL_BACKTRACE_BUFFER_LEN - 2] = '\n';
            backtrace_buffer[MPL_BACKTRACE_BUFFER_LEN - 1] = '\0';
            break;
        }
        chars += ret;
    }
    fprintf(output, "%s", backtrace_buffer);
}

#elif defined MPL_HAVE_BACKTRACE_SYMBOLS
static inline void backtrace_libc(FILE * output)
{
#ifndef MPL_MAX_TRACE_DEPTH
#define MPL_MAX_TRACE_DEPTH 32
#endif
    void *trace[MPL_MAX_TRACE_DEPTH];
    char **stack_strs;
    char backtrace_buffer[MPL_BACKTRACE_BUFFER_LEN];
    int frames, i, ret, chars = 0;

    frames = backtrace(trace, MPL_MAX_TRACE_DEPTH);
    stack_strs = backtrace_symbols(trace, frames);

    for (i = 0; i < frames; i++) {
        ret = MPL_snprintf(backtrace_buffer + chars,
                           MPL_BACKTRACE_BUFFER_LEN - chars, "%s\n", stack_strs[i]);
        if (ret + chars >= MPL_BACKTRACE_BUFFER_LEN) {
            /* the extra new line will be more readable than a merely
             * truncated string */
            backtrace_buffer[MPL_BACKTRACE_BUFFER_LEN - 2] = '\n';
            backtrace_buffer[MPL_BACKTRACE_BUFFER_LEN - 1] = '\0';
            break;
        }
        chars += ret;
    }
    fprintf(output, "%s", backtrace_buffer);
    free(stack_strs);
}
#else
static inline void backtrace_unsupported(FILE * output)
{
    fprintf(output, "No backtrace info available\n");
}
#endif

/* Pick one of the many ways one could dump out a call stack*/
void MPL_backtrace_show(FILE * output)
{
#ifdef MPL_HAVE_LIBBACKTRACE
    backtrace_libback(output);
#elif defined MPL_HAVE_LIBUNWIND
    /* libunwind is not able to get line numbers without forking off to
     * addr2line (?)*/
    backtrace_libunwind(output);
#elif defined MPL_HAVE_BACKTRACE_SYMBOLS
    backtrace_libc(output);
#else
    backtrace_unsupported(output);
#endif
}
