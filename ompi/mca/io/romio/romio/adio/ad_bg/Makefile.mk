## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_BG

AM_CPPFLAGS += -DBGL_OPTIM_STEP1_2=1 -DBGL_OPTIM_STEP1_1=1

noinst_HEADERS +=                                                    \
    adio/ad_bg/ad_bg_aggrs.h                                         \
    adio/ad_bg/ad_bg.h                                               \
    adio/ad_bg/ad_bg_pset.h                                          \
    adio/ad_bg/ad_bg_tuning.h

romio_other_sources +=                                               \
    adio/ad_bg/ad_bg_aggrs.c                                         \
    adio/ad_bg/ad_bg_close.c                                         \
    adio/ad_bg/ad_bg_flush.c                                         \
    adio/ad_bg/ad_bg_hints.c                                         \
    adio/ad_bg/ad_bg_pset.c                                          \
    adio/ad_bg/ad_bg_read.c                                          \
    adio/ad_bg/ad_bg_tuning.c                                        \
    adio/ad_bg/ad_bg_write.c                                         \
    adio/ad_bg/ad_bg.c                                               \
    adio/ad_bg/ad_bg_fcntl.c                                         \
    adio/ad_bg/ad_bg_getsh.c                                         \
    adio/ad_bg/ad_bg_open.c                                          \
    adio/ad_bg/ad_bg_rdcoll.c                                        \
    adio/ad_bg/ad_bg_setsh.c                                         \
    adio/ad_bg/ad_bg_wrcoll.c

endif BUILD_AD_BG
