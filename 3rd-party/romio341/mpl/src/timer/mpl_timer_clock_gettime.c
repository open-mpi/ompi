/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__CLOCK_GETTIME

#include "mpl_timer_common.h"
static time_t time_epoch;
static int is_initialized = 0;

int MPL_wtime(MPL_time_t * timeval)
{
    clock_gettime(CLOCK_REALTIME, timeval);

    return MPL_SUCCESS;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = ((double) (t2->tv_sec - t1->tv_sec) + 1.0e-9 * (double) (t2->tv_nsec - t1->tv_nsec));

    return MPL_SUCCESS;
}

int MPL_wtime_touint(MPL_time_t * t, unsigned int *val)
{
    *val = (unsigned int) t->tv_nsec;

    return MPL_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = ((double) (t->tv_sec - time_epoch) + 1.0e-9 * (double) (t->tv_nsec));

    return MPL_SUCCESS;
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

    return MPL_SUCCESS;
}

int MPL_wtick(double *wtick)
{
    struct timespec res;
    int rc;

    /* May return -1 for unimplemented.  If not implemented (POSIX
     * allows that), then we need to return the generic tick value. */
    rc = clock_getres(CLOCK_REALTIME, &res);
    if (!rc)
        *wtick = res.tv_sec + 1.0e-9 * res.tv_nsec;
    else
        *wtick = tickval;

    return MPL_SUCCESS;
}

int MPL_wtime_init(void)
{
    if (is_initialized)
        goto fn_exit;

    /* set a closer time_epoch so MPL_wtime_todouble retain ns resolution */
    /* time across process are still relavant within 1 hour */
    MPL_time_t t;
    MPL_wtime(&t);
    time_epoch = t.tv_sec - t.tv_sec % (3600);

    init_wtick();

    is_initialized = 1;

  fn_exit:
    return MPL_SUCCESS;
}

#endif
