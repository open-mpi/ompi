/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_TIMER_DEVICE_H_INCLUDED
#define MPL_TIMER_DEVICE_H_INCLUDED

#define MPLI_WTIME_IS_A_FUNCTION

/*
 * If the timer capability is provided by the device, it would need to
 * expose it in two ways:
 *
 *    1. It would expose it through the MPID_ functionality.  This
 *       model would be used by the MPICH internal code, and would
 *       give fast access to the timers.
 *
 *    2. It would expose it through the MPL_timer_ function pointers.
 *       This model would be used by "external" code segments (such as
 *       MPL) which do not have direct access to the MPICH devices.
 *       This model might be slightly slower, but would give the same
 *       functionality.
 */
extern int (*MPL_wtime_fn) (MPL_time_t * timeval);
extern int (*MPL_wtime_diff_fn) (MPL_time_t * t1, MPL_time_t * t2, double *diff);
extern int (*MPL_wtime_acc_fn) (MPL_time_t * t1, MPL_time_t * t2, MPL_time_t * t3);
extern int (*MPL_wtime_todouble_fn) (MPL_time_t * timeval, double *seconds);
extern int (*MPL_wtick_fn) (double *tick);

#endif /* MPL_TIMER_DEVICE_H_INCLUDED */
