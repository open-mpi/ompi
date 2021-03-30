/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#if MPL_TIMER_KIND == MPL_TIMER_KIND__PPC64_CYCLE

#include <sys/time.h>

static double seconds_per_tick = 0.0;
static uint64_t clockMHz = 0;
static int is_initialized = 0;

static uint64_t timeGetTime(void)
{
    struct timeval tv;
    gettimeofday(&tv, 0);
    return tv.tv_sec * 1000000ULL + tv.tv_usec;
}

static inline uint64_t getClockMHz()
{
    if (clockMHz == 0) {
        uint64_t sampleTime = 100ULL;   //sample time in usec
        uint64_t timeStart = 0ULL, timeStop = 0ULL;
        uint64_t startBase = 0ULL, endBase = 0ULL;
        uint64_t overhead = 0ULL, tbf = 0ULL, tbi = 0ULL;
        uint64_t ticks = 0ULL;
        int iter = 0ULL;

        do {
            tbi = tb();
            tbf = tb();
            tbi = tb();
            tbf = tb();

            overhead = tbf - tbi;
            timeStart = timeGetTime();

            while (timeGetTime() == timeStart)
                timeStart = timeGetTime();

            while (1) {
                timeStop = timeGetTime();

                if ((timeStop - timeStart) > 1) {
                    startBase = tb();
                    break;
                }
            }

            timeStart = timeStop;

            while (1) {
                timeStop = timeGetTime();

                if ((timeStop - timeStart) > sampleTime) {
                    endBase = tb();
                    break;
                }
            }

            ticks = ((endBase - startBase) + (overhead));
            iter++;

            if (iter == 10ULL) {
                fprintf(stderr, "Warning: unable to initialize high resolution timer.\n");
                return -1;
            }
        }
        while (endBase <= startBase);

        return (uint64_t) (ticks / sampleTime);
    } else
        return clockMHz;
}

int MPL_wtick(double *wtick)
{
    *wtick = seconds_per_tick;

    return MPL_SUCCESS;
}

int MPL_wtime_init(void)
{
    int rc = MPL_SUCCESS;

    if (is_initialized)
        goto fn_exit;

    clockMHz = getClockMHz();
    seconds_per_tick = 1.0 / ((double) clockMHz * 1000000.0);
    if (clockMHz == -1ULL)
        rc = MPL_ERR_TIMER_NOT_INITIALIZED;

    is_initialized = 1;

  fn_exit:
    return rc;
}

int MPL_wtime_diff(MPL_time_t * t1, MPL_time_t * t2, double *diff)
{
    *diff = (double) (*t2 - *t1) * seconds_per_tick;

    return MPL_SUCCESS;
}

int MPL_wtime_touint(MPL_time_t * t, unsigned int *val)
{
    *val = (unsigned int) (*t & 0xffffffffUL);

    return MPL_SUCCESS;
}

int MPL_wtime_todouble(MPL_time_t * t, double *val)
{
    *val = (double) *t * seconds_per_tick;

    return MPL_SUCCESS;
}

int MPL_wtime_acc(MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3)
{
    *t3 += (*t2 - *t1);

    return MPL_SUCCESS;
}

#endif
