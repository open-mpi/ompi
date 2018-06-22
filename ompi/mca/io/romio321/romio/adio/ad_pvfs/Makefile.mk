## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_PVFS

noinst_HEADERS += adio/ad_pvfs/ad_pvfs.h

romio_other_sources +=            \
    adio/ad_pvfs/ad_pvfs_close.c  \
    adio/ad_pvfs/ad_pvfs_read.c   \
    adio/ad_pvfs/ad_pvfs_open.c   \
    adio/ad_pvfs/ad_pvfs_write.c  \
    adio/ad_pvfs/ad_pvfs_fcntl.c  \
    adio/ad_pvfs/ad_pvfs_flush.c  \
    adio/ad_pvfs/ad_pvfs_resize.c \
    adio/ad_pvfs/ad_pvfs_hints.c  \
    adio/ad_pvfs/ad_pvfs_delete.c \
    adio/ad_pvfs/ad_pvfs.c

endif BUILD_AD_PVFS


