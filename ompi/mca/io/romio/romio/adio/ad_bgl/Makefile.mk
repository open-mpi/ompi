## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_BGL

noinst_HEADERS +=               \
    adio/ad_bgl/ad_bgl.h        \
    adio/ad_bgl/ad_bgl_aggrs.h  \
    adio/ad_bgl/ad_bgl_pset.h   \
    adio/ad_bgl/ad_bgl_tuning.h

romio_other_sources +=          \
    adio/ad_bgl/ad_bgl_open.c   \
    adio/ad_bgl/ad_bgl_close.c  \
    adio/ad_bgl/ad_bgl_fcntl.c  \
    adio/ad_bgl/ad_bgl_flush.c  \
    adio/ad_bgl/ad_bgl_read.c   \
    adio/ad_bgl/ad_bgl_write.c  \
    adio/ad_bgl/ad_bgl_getsh.c  \
    adio/ad_bgl/ad_bgl_setsh.c  \
    adio/ad_bgl/ad_bgl.c        \
    adio/ad_bgl/ad_bgl_aggrs.c  \
    adio/ad_bgl/ad_bgl_pset.c   \
    adio/ad_bgl/ad_bgl_hints.c  \
    adio/ad_bgl/ad_bgl_rdcoll.c \
    adio/ad_bgl/ad_bgl_wrcoll.c \
    adio/ad_bgl/ad_bgl_tuning.c

endif BUILD_AD_BGL

