/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

/* style: allow:vprintf:1 sig:0 */
/* style: allow:vfprintf:4 sig:0 */
/* style: allow:fprintf:2 sig:0 */

int MPL_usage_printf(const char *str, ...)
{
    int n;
    va_list list;
    const char *format_str;

    va_start(list, str);
    format_str = str;
    n = vprintf(format_str, list);
    va_end(list);

    fflush(stdout);

    return n;
}

int MPL_internal_error_printf(const char *str, ...)
{
    int n;
    va_list list;
    const char *format_str;

    va_start(list, str);
    format_str = str;
    n = vfprintf(stderr, format_str, list);
    va_end(list);

    fflush(stderr);

    return n;
}

/* Like internal_error_printf, but for the system routine name with
   errno errnum.  Str may be null */
int MPL_internal_sys_error_printf(const char *name, int errnum, const char *str, ...)
{
    int n = 0;
    va_list list;
    const char *format_str = 0;

    /* Prepend information on the system error */
    if (!format_str)
        format_str = "Error in system call %s: %s\n";

    fprintf(stderr, format_str, name, MPL_strerror(errnum));

    /* Now add the message that is specific to this use, if any */
    if (str) {
        va_start(list, str);
        format_str = str;
        n = vfprintf(stderr, format_str, list);
        va_end(list);
    }

    fflush(stderr);

    return n;
}

int MPL_msg_printf(const char *str, ...)
{
    int n;
    va_list list;
    const char *format_str;

    va_start(list, str);
    format_str = str;
    n = vfprintf(stdout, format_str, list);
    va_end(list);

    fflush(stdout);

    return n;
}

void MPL_exit(int exit_code)
{
    exit(exit_code);
}
