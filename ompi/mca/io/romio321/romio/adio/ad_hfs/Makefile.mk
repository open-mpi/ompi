## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_HFS

noinst_HEADERS += adio/ad_hfs/ad_hfs.h

romio_other_sources +=          \
    adio/ad_hfs/ad_hfs_read.c   \
    adio/ad_hfs/ad_hfs_open.c   \
    adio/ad_hfs/ad_hfs_write.c  \
    adio/ad_hfs/ad_hfs_fcntl.c  \
    adio/ad_hfs/ad_hfs_resize.c \
    adio/ad_hfs/ad_hfs.c

endif BUILD_AD_HFS

