/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__CLOCK_GETTIME

int MPL_wtime(MPL_time_t * timeval)
{
    /* POSIX timer (14.2.1, page 311) */
    clock_gettime(CLOCK_REALTIME, timeval);

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = ((double) (t2->tv_sec - t1->tv_sec) + 1.0e-9 * (double) (t2->tv_nsec - t1->tv_nsec));

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = ((double) t->tv_sec + 1.0e-9 * (double) t->tv_nsec);

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    long nsec, sec;

    nsec = t2->tv_nsec - t1->tv_nsec;
    sec = t2->tv_sec - t1->tv_sec;

    t3->tv_sec += sec;
    t3->tv_nsec += nsec;
    while (t3->tv_nsec > 1000000000) {
        t3->tv_nsec -= 1000000000;
        t3->tv_sec++;
    }

    return MPL_TIMER_SUCCESS;
}

int MPL_wtick(double *wtick)
{
    struct timespec res;
    int rc;

    rc = clock_getres(CLOCK_REALTIME, &res);
    if (!rc)
        /* May return -1 for unimplemented ! */
        *wtick = res.tv_sec + 1.0e-9 * res.tv_nsec;

    /* Sigh.  If not implemented (POSIX allows that),
     * then we need to return the generic tick value */
    *wtick = tickval;

    return MPL_TIMER_SUCCESS;
}

int MPL_wtime_init(void)
{
    init_wtick();

    return MPL_TIMER_SUCCESS;
}

#endif
