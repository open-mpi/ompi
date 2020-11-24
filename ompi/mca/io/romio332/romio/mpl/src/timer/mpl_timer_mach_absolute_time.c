/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__MACH_ABSOLUTE_TIME

static double MPIR_Wtime_mult;

int MPL_wtime_init(void)
{
    mach_timebase_info_data_t info;
    mach_timebase_info(&info);
    MPIR_Wtime_mult = 1.0e-9 * ((double) info.numer / (double) info.denom);
    init_wtick();

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime(MPL_time_t * timeval)
{
    *timeval = mach_absolute_time();

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = (*t2 - *t1) * MPIR_Wtime_mult;

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = *t * MPIR_Wtime_mult;

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    *t3 += *t2 - *t1;

    return MPL_TIMER_SUCCESS;
}

int MPL_wtick(double *wtick)
{
    *wtick = tickval;

    return MPL_TIMER_SUCCESS;
}

#endif
