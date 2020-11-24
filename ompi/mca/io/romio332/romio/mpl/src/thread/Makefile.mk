## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2016 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

lib@MPLLIBNAME@_la_SOURCES +=        \
    src/thread/mpl_thread.c        \
    src/thread/mpl_thread_win.c    \
    src/thread/mpl_thread_solaris.c    \
    src/thread/mpl_thread_argobots.c    \
    src/thread/mpl_thread_posix.c
