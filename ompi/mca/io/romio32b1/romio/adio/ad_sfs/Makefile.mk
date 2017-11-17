## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_SFS

noinst_HEADERS += adio/ad_sfs/ad_sfs.h

romio_other_sources +=         \
    adio/ad_sfs/ad_sfs_open.c  \
    adio/ad_sfs/ad_sfs_fcntl.c \
    adio/ad_sfs/ad_sfs_flush.c \
    adio/ad_sfs/ad_sfs.c

endif BUILD_AD_SFS

