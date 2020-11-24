## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2016 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

lib@MPLLIBNAME@_la_SOURCES +=        \
    src/shm/mpl_shm.c                \
    src/shm/mpl_shm_sysv.c            \
    src/shm/mpl_shm_mmap.c            \
    src/shm/mpl_shm_win.c
