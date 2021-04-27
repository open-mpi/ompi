/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__MACH_ABSOLUTE_TIME

#include "mpl_timer_common.h"

static double wtime_mult;
static int is_initialized = 0;

int MPL_wtime_init(void)
{
    if (is_initialized)
        goto fn_exit;

    mach_timebase_info_data_t info;
    mach_timebase_info(&info);
    wtime_mult = 1.0e-9 * ((double) info.numer / (double) info.denom);
    init_wtick();

    is_initialized = 1;

  fn_exit:
    return MPL_SUCCESS;
}

int MPL_wtime(MPL_time_t * timeval)
{
    *timeval = mach_absolute_time();

    return MPL_SUCCESS;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = (*t2 - *t1) * wtime_mult;

    return MPL_SUCCESS;
}

int MPL_wtime_touint(MPL_time_t * t, unsigned int *val)
{
    *val = (unsigned int) (*t & 0xffffffffUL);

    return MPL_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = *t * wtime_mult;

    return MPL_SUCCESS;
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    *t3 += *t2 - *t1;

    return MPL_SUCCESS;
}

int MPL_wtick(double *wtick)
{
    *wtick = tickval;

    return MPL_SUCCESS;
}

#endif
