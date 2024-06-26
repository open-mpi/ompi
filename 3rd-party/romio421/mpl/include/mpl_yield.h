/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_YIELD_H_INCLUDED
#define MPL_YIELD_H_INCLUDED

#include "mplconfig.h"

/* MPL_SCHED_YIELD() - Yield the processor to OS scheduler */
/* On a typical Linux system (verified with kernels 3.2 and 3.5),
 * usleep has a resolution of more than 1000 cycles. This makes
 * it impractical if the desired sleeping period is shorter. On
 * the other hand, sleep(0) returns immediately without going to
 * the kernel. This means that there is no actual yielding, which
 * is equivalent to doing nothing. Thus, usleep and sleep are not
 * recommended as ways to yield the CPU, and sched_yield would be
 * preferred if available.
 * Note that nanosleep has the same shortcomings as usleep.*/

#if defined(MPL_USE_SWITCHTOTHREAD_FOR_YIELD)
#include <winsock2.h>
#include <windows.h>
#define MPL_sched_yield() SwitchToThread()
#elif defined(MPL_USE_WIN32_SLEEP_FOR_YIELD)
#include <winsock2.h>
#include <windows.h>
#define MPL_sched_yield() Sleep(0)
#elif defined(MPL_USE_SCHED_YIELD_FOR_YIELD)
#ifdef MPL_HAVE_SCHED_H
#include <sched.h>
#endif
#define MPL_sched_yield() sched_yield()
#elif defined(MPL_USE_YIELD_FOR_YIELD)
#ifdef MPL_HAVE_SCHED_H
#include <sched.h>
#endif
#define MPL_sched_yield() yield()
#elif defined (MPL_USE_SELECT_FOR_YIELD)
#ifdef MPL_HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#define MPL_sched_yield() do { struct timeval t; t.tv_sec = 0; t.tv_usec = 0; select(0,0,0,0,&t); } while (0)
#elif defined (MPL_USE_USLEEP_FOR_YIELD)
#ifdef MPL_HAVE_UNISTD_H
#include <unistd.h>
#if defined (MPL_NEEDS_USLEEP_DECL)
int usleep(useconds_t usec);
#endif
#endif
#define MPL_sched_yield() usleep(0)
#elif defined (MPL_USE_SLEEP_FOR_YIELD)
#ifdef MPL_HAVE_UNISTD_H
#include <unistd.h>
#endif
#define MPL_sched_yield() sleep(0)
#elif defined (MPL_USE_NOTHING_FOR_YIELD)
#define MPL_sched_yield() do {} while (0)
#else
#error "No mechanism available to yield"
#endif

#endif /* MPL_YIELD_H_INCLUDED */
