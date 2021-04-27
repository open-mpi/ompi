/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_TIMER_CLOCK_GETTIME_H_INCLUDED
#define MPL_TIMER_CLOCK_GETTIME_H_INCLUDED

#define MPLI_WTIME_IS_A_FUNCTION

#include <time.h>
#ifdef MPL_NEEDS_SYS_TIME_H
/* Some OS'es mistakenly require sys/time.h to get the definition of
   CLOCK_REALTIME (POSIX requires the definition to be in time.h) */
#include <sys/time.h>
#endif

#endif /* MPL_TIMER_CLOCK_GETTIME_H_INCLUDED */
