/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_TIMER_COMMON_H_INCLUDED
#define MPL_TIMER_COMMON_H_INCLUDED

#include "mpl_timer.h"

static double tickval = -1.0;

/*
 * For timers that do not have defined resolutions, compute the resolution
 * by sampling the clock itself.
 *
 */
static void init_wtick(void)
{
    double timediff;
    MPL_time_t t1, t2;
    int cnt;
    int icnt;

    tickval = 1.0e6;
    for (icnt = 0; icnt < 10; icnt++) {
        cnt = 1000;
        MPL_wtime(&t1);
        do {
            MPL_wtime(&t2);
            MPL_wtime_diff(&t1, &t2, &timediff);
            if (timediff > 0)
                break;
        }
        while (cnt--);
        if (cnt && timediff > 0.0 && timediff < tickval) {
            MPL_wtime_diff(&t1, &t2, &tickval);
        }
    }
}

#endif /* MPL_TIMER_COMMON_H_INCLUDED */
