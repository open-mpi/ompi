/*
 * Copyright (C) 2014      Artem Polyakov <artpol84@gmail.com>
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

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

#include "opal/constants.h"
#include "opal/runtime/opal_params.h"


#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_list.h"
#include "opal/util/timings.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/mca/timer/timer.h"

#include MCA_timer_IMPLEMENTATION_HEADER

static double get_ts_gettimeofday(void)
{
    double ret;
    /* Use gettimeofday() if we opal wasn't initialized */
    struct timeval tv;
    gettimeofday(&tv, NULL);
    ret = tv.tv_sec;
    ret += (double)tv.tv_usec / 1000000.0;
    return ret;
}

#if OPAL_TIMER_CYCLE_NATIVE
static double get_ts_cycle(void)
{
        return ((double) opal_timer_base_get_cycles()) / opal_timer_base_get_freq();
}
#endif

#if OPAL_TIMER_USEC_NATIVE
static double get_ts_usec(void)
{
        return ((double) opal_timer_base_get_usec()) / 1000000.0;
}
#endif

opal_timing_ts_func_t opal_timing_ts_func(opal_timer_type_t type)
{
    switch (type) {
        case OPAL_TIMING_GET_TIME_OF_DAY:
            return get_ts_gettimeofday;

        case OPAL_TIMING_CYCLE_NATIVE:
#if OPAL_TIMER_CYCLE_NATIVE
           return get_ts_cycle;
#else
           return NULL;
#endif // OPAL_TIMER_CYCLE_NATIVE
        case OPAL_TIMING_USEC_NATIVE:
#if OPAL_TIMER_USEC_NATIVE
            return get_ts_usec;
#else
            return NULL;
#endif // OPAL_TIMER_USEC_NATIVE
        default:
            if( !opal_initialized ){
                return get_ts_gettimeofday;
            }
#if OPAL_TIMER_CYCLE_NATIVE
            return get_ts_cycle;
#elif OPAL_TIMER_USEC_NATIVE
            return get_ts_usec;
#endif
            return get_ts_gettimeofday;
    }
}

