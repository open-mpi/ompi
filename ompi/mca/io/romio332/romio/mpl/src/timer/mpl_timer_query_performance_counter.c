/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__QUERYPERFORMANCECOUNTER

static double seconds_per_tick = 0.0;   /* High performance counter frequency */

int MPL_wtime_init(void)
{
    LARGE_INTEGER n;
    QueryPerformanceFrequency(&n);
    seconds_per_tick = 1.0 / (double) n.QuadPart;
    return 0;
}

double MPL_wtick(void)
{
    return seconds_per_tick;
}

void MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = (double) t->QuadPart * seconds_per_tick;
}

void MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    LARGE_INTEGER n;
    n.QuadPart = t2->QuadPart - t1->QuadPart;
    *diff = (double) n.QuadPart * seconds_per_tick;
}

void MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    t3->QuadPart += ((t2->QuadPart) - (t1->QuadPart));
}

#endif
