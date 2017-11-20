## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_UFS

noinst_HEADERS += adio/ad_ufs/ad_ufs.h

romio_other_sources +=        \
    adio/ad_ufs/ad_ufs.c      \
    adio/ad_ufs/ad_ufs_open.c

endif BUILD_AD_UFS

