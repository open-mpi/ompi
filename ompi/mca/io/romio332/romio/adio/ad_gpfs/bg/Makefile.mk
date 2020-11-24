## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2012 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_BG

noinst_HEADERS +=                                                    \
    adio/ad_gpfs/bg/ad_bg_aggrs.h                                         \
    adio/ad_gpfs/bg/ad_bg_pset.h

romio_other_sources +=                                               \
    adio/ad_gpfs/bg/ad_bg_aggrs.c                                         \
    adio/ad_gpfs/bg/ad_bg_pset.c 

endif BUILD_AD_BG
