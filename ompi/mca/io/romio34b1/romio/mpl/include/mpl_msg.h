/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_MSG_H_INCLUDED
#define MPL_MSG_H_INCLUDED

#include "mpl.h"

#if defined(MPL_HAVE_MACRO_VA_ARGS)
#define MPL_error_printf(...) fprintf(stderr,__VA_ARGS__)
#else
#define MPL_error_printf printf
#endif

/* These routines are used to ensure that messages are sent to the
 * appropriate output and (eventually) are properly
 * internationalized */
int MPL_usage_printf(const char *str, ...) ATTRIBUTE((format(printf, 1, 2)));
int MPL_msg_printf(const char *str, ...) ATTRIBUTE((format(printf, 1, 2)));
int MPL_internal_error_printf(const char *str, ...) ATTRIBUTE((format(printf, 1, 2)));
int MPL_internal_sys_error_printf(const char *, int, const char *str,
                                  ...) ATTRIBUTE((format(printf, 3, 4)));
void MPL_exit(int);

#endif /* MPL_MSG_H_INCLUDED */
