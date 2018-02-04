/*
 * Copyright (C) 2014      Artem Polyakov <artpol84@gmail.com>
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2017-2018 Mellanox Technologies Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include <src/include/pmix_config.h>
#include <pmix_rename.h>

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>

#include <string.h>

#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include "src/class/pmix_pointer_array.h"
#include "src/class/pmix_list.h"

#include "src/util/output.h"
#include "src/util/basename.h"
#include "src/util/timings.h"

extern int pmix_initialized;

static double get_ts_gettimeofday(void)
{
    double ret;
    /* Use gettimeofday() if we pmix wasn't initialized */
    struct timeval tv;
    gettimeofday(&tv, NULL);
    ret = tv.tv_sec;
    ret += (double)tv.tv_usec / 1000000.0;
    return ret;
}

#if PMIX_TIMER_CYCLE_NATIVE
static double get_ts_cycle(void)
{
        return ((double) pmix_timer_base_get_cycles()) / pmix_timer_base_get_freq();
}
#endif

#if PMIX_TIMER_USEC_NATIVE
static double get_ts_usec(void)
{
        return ((double) pmix_timer_base_get_usec()) / 1000000.0;
}
#endif

pmix_timing_ts_func_t pmix_timing_ts_func(pmix_timer_type_t type)
{
    switch (type) {
        case PMIX_TIMING_GET_TIME_OF_DAY:
            return get_ts_gettimeofday;

        case PMIX_TIMING_CYCLE_NATIVE:
#if PMIX_TIMER_CYCLE_NATIVE
           return get_ts_cycle;
#else
           return NULL;
#endif // PMIX_TIMER_CYCLE_NATIVE
        case PMIX_TIMING_USEC_NATIVE:
#if PMIX_TIMER_USEC_NATIVE
            return get_ts_usec;
#else
            return NULL;
#endif // PMIX_TIMER_USEC_NATIVE
        default:
            if( !pmix_initialized ){
                return get_ts_gettimeofday;
            }
#if PMIX_TIMER_CYCLE_NATIVE
            return get_ts_cycle;
#elif PMIX_TIMER_USEC_NATIVE
            return get_ts_usec;
#endif
            return get_ts_gettimeofday;
    }
}

