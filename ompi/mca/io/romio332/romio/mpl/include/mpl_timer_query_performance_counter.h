/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_TIMER_QUERY_PERFORMANCE_COUNTER_H_INCLUDED
#define MPL_TIMER_QUERY_PERFORMANCE_COUNTER_H_INCLUDED

#include <winsock2.h>
#include <windows.h>

static inline void MPL_wtime(MPL_time_t * timeval)
{
    QueryPerformanceCounter(timeval);

    return MPL_TIMER_SUCCESS;
}

#endif /* MPL_TIMER_QUERY_PERFORMANCE_COUNTER_H_INCLUDED */
