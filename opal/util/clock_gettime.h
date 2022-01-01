/*
 * Copyright (c) 2022 Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file clock_gettime.h
 *
 * Simple, portable wrappers around clock_gettime(3) and
 * clock_getres(3) to always get monotonically-increasing time.
 *
 * If the underlying OS does not have clock_gettime(3), use
 * gettimeofday(3) instead.
 *
 * We intentionally do not use the OPAL timer framework for
 * high-prevision time here; see
 * https://github.com/open-mpi/ompi/issues/3003 for more details.
 *
 * As of Dec 2021, it turns out that CLOCK_MONOTONIC can actually go
 * backwards on macOS (!).  CLOCK_MONOTONIC does *not* go backwards on
 * Linux (or anywhere else we can find), though, even in the presence
 * of small NTP time adjustments -- e.g., adjtime(3) simply slightly
 * speeds up or slows down the system clock to make it eventually get
 * to the desired time.  On macOS, we can use CLOCK_MONOTONIC_RAW,
 * which never goes backwards.
 *
 * Hence, for these wrappers, use CLOCK_MONOTONIC_RAW on Darwin, and
 * use CLOCK_MONOTONIC everywhere else.
 *
 * See
 * https://github.com/open-mpi/ompi/pull/8057#discussion_r762612710
 * and
 * https://github.com/open-mpi/ompi/pull/8057#discussion_r762618783
 * for more details.
 */

#ifndef OPAL_UTIL_CLOCK_GETTIME_H_
#define OPAL_UTIL_CLOCK_GETTIME_H_

#include "opal_config.h"

#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if OPAL_HAVE_CLOCK_GETTIME
#if defined(__darwin__)
#define OPAL_CLOCK_TYPE CLOCK_MONOTONIC_RAW
#else
#define OPAL_CLOCK_TYPE CLOCK_MONOTONIC
#endif
#endif // OPAL_HAVE_CLOCK_GETTIME

#if !defined(HAVE_STRUCT_TIMESPEC_TV_NSEC)
// Make sure that we have struct timespec; if not, define it.
struct timespec {
    time_t tv_sec;
    long tv_nsec;
};
#endif

/**
 * Simple, portable wrapper around clock_gettime(3) for high-precision time.
 *
 * If the underlying system does not have clock_gettime(3), use
 * gettimeofday(3) instead.
 *
 * @param spec (OUT) Struct to return the time
 * @return Return value from underlying clock_gettime()
 */
static inline int opal_clock_gettime(struct timespec *spec)
{
#if OPAL_HAVE_CLOCK_GETTIME
    return clock_gettime(OPAL_CLOCK_TYPE, spec);
#else
    // If we do not have clock_gettime(), fall back to gettimeofday()
    struct timeval tv;
    int ret = gettimeofday(&tv, NULL);

    spec->tv_sec = tv.tv_sec;
    // Elevate the micrseconds to nanoseconds
    spec->tv_nsec = tv.tv_usec * 1000;

    return ret;
#endif
}

/**
 * Simple, portable wrapper around clock_getres(3) for high-precision time.
 *
 * If the underlying system does not have clock_gettime(3), return usec
 * precison (because opal_clock_gettime() will be using gettimeofday(3)).
 *
 * @param spec (OUT) Struct to return the resolution
 * @return Return value from underlying clock_getres()
 */
static inline int opal_clock_getres(struct timespec *spec)
{
#if OPAL_HAVE_CLOCK_GETTIME
    return clock_getres(OPAL_CLOCK_TYPE, spec);
#else
    // If we don't have clock_gettime(), just return usec precision.
    spec->tv_sec = 0;
    spec->tv_nsec = 1000;

    return 0;
#endif
}

#endif // OPAL_UTIL_CLOCK_GETTIME_H_
