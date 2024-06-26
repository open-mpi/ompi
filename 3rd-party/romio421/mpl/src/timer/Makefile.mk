##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

noinst_HEADERS += src/timer/mpl_timer_common.h

lib@MPLLIBNAME@_la_SOURCES += \
    src/timer/mpl_timer_clock_gettime.c \
    src/timer/mpl_timer_gcc_ia64_cycle.c \
    src/timer/mpl_timer_gethrtime.c \
    src/timer/mpl_timer_gettimeofday.c \
    src/timer/mpl_timer_linux86_cycle.c \
    src/timer/mpl_timer_ppc64_cycle.c \
    src/timer/mpl_timer_mach_absolute_time.c
