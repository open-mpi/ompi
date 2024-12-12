/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__GETTIMEOFDAY

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mpl_timer_common.h"

static int is_initialized = 0;

int MPL_wtime(MPL_time_t * tval)
{
    gettimeofday(tval, NULL);

    return MPL_SUCCESS;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = ((double) (t2->tv_sec - t1->tv_sec) + .000001 * (double) (t2->tv_usec - t1->tv_usec));

    return MPL_SUCCESS;
}

int MPL_wtime_touint(MPL_time_t * t, unsigned int *val)
{
    *val = (unsigned int) t->tv_usec;

    return MPL_SUCCESS;
}

int MPL_wtime_to_ticks(MPL_time_t * t, long long int *val)
{
    *val = t->tv_sec * 1000000;
    *val += t->tv_usec;

    return MPL_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = (double) t->tv_sec + .000001 * (double) t->tv_usec;

    return MPL_SUCCESS;
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    int usec, sec;

    usec = t2->tv_usec - t1->tv_usec;
    sec = t2->tv_sec - t1->tv_sec;
    t3->tv_usec += usec;
    t3->tv_sec += sec;
    /* Handle carry to the integer seconds field */
    while (t3->tv_usec > 1000000) {
        t3->tv_usec -= 1000000;
        t3->tv_sec++;
    }

    return MPL_SUCCESS;
}

int MPL_wtick(double *wtick)
{
    *wtick = tickval;

    return MPL_SUCCESS;
}

int MPL_ticks_per_second(long long int *ticks_per_second)
{
    /* microseconds */
    *ticks_per_second = 1000000;

    return MPL_SUCCESS;
}

int MPL_wtime_init(void)
{
    if (is_initialized)
        goto fn_exit;

    init_wtick();

    is_initialized = 1;

  fn_exit:
    return MPL_SUCCESS;
}

#endif
