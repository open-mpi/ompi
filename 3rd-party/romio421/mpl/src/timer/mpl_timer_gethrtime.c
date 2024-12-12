/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__GETHRTIME

static int is_initialized = 0;

/*
 * MPL_time_t is hrtime_t, which under Solaris is defined as a 64bit
 * longlong_t .  However, the Solaris header files will define
 * longlong_t as a structure in some circumstances, making arithmetic
 * with hrtime_t invalid.  FIXME.
 * To fix this, we'll need to test hrtime_t arithmetic in the configure
 * program, and if it fails, check for the Solaris definitions (
 * union { double _d; int32_t _l[2]; }.  Alternately, we may decide that
 * if hrtime_t is not supported, then neither is gethrtime.
 *
 * Note that the Solaris sys/types.h file *assumes* that no other compiler
 * supports an 8 byte long long.  We can also cast hrtime_t to long long
 * if long long is available and 8 bytes.
 */
int MPL_wtime(MPL_time_t * timeval)
{
    *timeval = gethrtime();

    return MPL_SUCCESS;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = 1.0e-9 * (double) (*t2 - *t1);

    return MPL_SUCCESS;
}

int MPL_wtime_touint(MPL_time_t * t, unsigned int *val)
{
    *val = (unsigned int) (*t & 0xffffffffUL);

    return MPL_SUCCESS;
}

int MPL_wtime_to_ticks(MPL_time_t * t, long long int *val)
{
    *val = *t;

    return MPL_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = 1.0e-9 * (*t);

    return MPL_SUCCESS;
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    *t3 += ((*t2) - (*t1));

    return MPL_SUCCESS;
}

int MPL_wtick(double *wtick)
{
    /* According to the documentation, ticks should be in nanoseconds.  This
     * is untested */
    *wtick = 1.0e-9;

    return MPL_SUCCESS;
}

int MPL_ticks_per_second(long long int *ticks_per_second)
{
    /* nanoseconds */
    *ticks_per_second = 1000000000;

    return MPL_SUCCESS;
}

int MPL_wtime_init(void)
{
    if (is_initialized)
        goto fn_exit;

    is_initialized = 1;

  fn_exit:
    return MPL_SUCCESS;
}

#endif
