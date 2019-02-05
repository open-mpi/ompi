/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_TIMER_DARWIN_TIMER_DARWIN_H
#define OPAL_MCA_TIMER_DARWIN_TIMER_DARWIN_H

#include "opal_config.h"
#include <mach/mach_time.h>

typedef uint64_t opal_threadss_t;

/* frequency in mhz */
OPAL_DECLSPEC extern opal_threadss_t opal_threadss_darwin_freq;
OPAL_DECLSPEC extern mach_timebase_info_data_t opal_threadss_darwin_info;
OPAL_DECLSPEC extern opal_threadss_t opal_threadss_darwin_bias;

/**
 * Use the pragmatic solution proposed at
 * http://stackoverflow.com/questions/23378063/how-can-i-use-mach-absolute-time-without-overflowing/23378064#23378064
 */
static inline opal_threadss_t
opal_threadss_base_get_cycles(void)
{
    uint64_t now = mach_absolute_time();

    if( opal_threadss_darwin_info.denom == 0 ) {
        (void)mach_timebase_info(&opal_threadss_darwin_info);
        if( opal_threadss_darwin_info.denom > 1024 ) {
            double frac = (double)opal_threadss_darwin_info.numer/opal_threadss_darwin_info.denom;
            opal_threadss_darwin_info.denom = 1024;
            opal_threadss_darwin_info.numer = opal_threadss_darwin_info.denom * frac + 0.5;
        }
        opal_threadss_darwin_bias = now;
    }
    /* this is basically a wrapper around the "right" assembly to convert
       the tick counter off the PowerPC Time Base into nanos. */
    return (now - opal_threadss_darwin_bias) * opal_threadss_darwin_info.numer / opal_threadss_darwin_info.denom;
}


static inline opal_threadss_t
opal_threadss_base_get_usec(void)
{
    /* freq is in Hz, so this gives usec */
    return opal_threadss_base_get_cycles() / 1000;
}


static inline opal_threadss_t
opal_threadss_base_get_freq(void)
{
    return opal_threadss_darwin_freq;
}

typedef pthread_key_t opal_tsd_key_t;

static inline int
opal_tsd_key_delete(opal_tsd_key_t key)
{
    return pthread_key_delete(key);
}

static inline int
opal_tsd_setspecific(opal_tsd_key_t key, void *value)
{
    return pthread_setspecific(key, value);
}

static inline int
opal_tsd_getspecific(opal_tsd_key_t key, void **valuep)
{
    *valuep = pthread_getspecific(key);
    return OPAL_SUCCESS;
}

#define OPAL_TIMER_CYCLE_NATIVE 0
#define OPAL_TIMER_CYCLE_SUPPORTED 1
#define OPAL_TIMER_USEC_NATIVE 1
#define OPAL_TIMER_USEC_SUPPORTED 1

#endif
