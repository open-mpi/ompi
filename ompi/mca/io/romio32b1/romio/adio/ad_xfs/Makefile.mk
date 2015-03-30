## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_XFS

noinst_HEADERS += adio/ad_xfs/ad_xfs.h

romio_other_sources +=          \
    adio/ad_xfs/ad_xfs.c        \
    adio/ad_xfs/ad_xfs_fcntl.c  \
    adio/ad_xfs/ad_xfs_hints.c  \
    adio/ad_xfs/ad_xfs_open.c   \
    adio/ad_xfs/ad_xfs_read.c   \
    adio/ad_xfs/ad_xfs_resize.c \
    adio/ad_xfs/ad_xfs_write.c

endif BUILD_AD_XFS

