## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2016 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

lib@MPLLIBNAME@_la_SOURCES += \
    src/timer/mpl_timer_clock_gettime.c \
    src/timer/mpl_timer_gcc_ia64_cycle.c \
    src/timer/mpl_timer_gethrtime.c \
    src/timer/mpl_timer_gettimeofday.c \
    src/timer/mpl_timer_linux86_cycle.c \
    src/timer/mpl_timer_ppc64_cycle.c \
    src/timer/mpl_timer_mach_absolute_time.c \
    src/timer/mpl_timer_query_performance_counter.c \
    src/timer/mpl_timer_win86_cycle.c \
    src/timer/mpl_timer_device.c
