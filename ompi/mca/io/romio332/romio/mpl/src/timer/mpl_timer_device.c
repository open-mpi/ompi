/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__DEVICE

int (*MPL_wtime_fn) (MPL_time_t * timeval) = NULL;
int (*MPL_wtime_diff_fn) (MPL_time_t * t1, MPL_time_t * t2, double *diff) = NULL;
int (*MPL_wtime_acc_fn) (MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3) = NULL;
int (*MPL_wtime_todouble_fn) (MPL_time_t * timeval, double *seconds) = NULL;
int (*MPL_wtick_fn) (double *tick) = NULL;

int MPL_wtime(MPL_time_t * timeval)
{
    if (MPL_wtime_fn == NULL)
        return MPL_TIMER_ERR_NOT_INITIALIZED;
    return MPL_wtime_fn(timeval);
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    if (MPL_wtime_diff_fn == NULL)
        return MPL_TIMER_ERR_NOT_INITIALIZED;
    return MPL_wtime_diff_fn(t1, t2, diff);
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    if (MPL_wtime_todouble_fn == NULL)
        return MPL_TIMER_ERR_NOT_INITIALIZED;
    return MPL_wtime_todouble_fn(t, val);
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    if (MPL_wtime_acc_fn == NULL)
        return MPL_TIMER_ERR_NOT_INITIALIZED;
    return MPL_wtime_acc_fn(t1, t2, t3);
}

int MPL_wtick(double *wtick)
{
    if (MPL_wtick_fn == NULL)
        return MPL_TIMER_ERR_NOT_INITIALIZED;
    return MPL_wtick_fn(wtick);
}

#endif
