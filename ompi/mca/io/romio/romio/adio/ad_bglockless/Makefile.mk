## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_BGLOCKLESS

noinst_HEADERS += adio/ad_bglockless/ad_bglockless.h

romio_other_sources +=                          \
    adio/ad_bglockless/ad_bglockless.c          \
    adio/ad_bglockless/ad_bglockless_features.c

endif BUILD_AD_BGLOCKLESS

