## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_ZOIDFS

noinst_HEADERS += adio/ad_zoidfs/ad_zoidfs.h adio/ad_zoidfs/ad_zoidfs_common.h

romio_other_sources +=                    \
    adio/ad_zoidfs/ad_zoidfs.c            \
    adio/ad_zoidfs/ad_zoidfs_close.c      \
    adio/ad_zoidfs/ad_zoidfs_common.c     \
    adio/ad_zoidfs/ad_zoidfs_delete.c     \
    adio/ad_zoidfs/ad_zoidfs_fcntl.c      \
    adio/ad_zoidfs/ad_zoidfs_flush.c      \
    adio/ad_zoidfs/ad_zoidfs_io.c         \
    adio/ad_zoidfs/ad_zoidfs_open.c       \
    adio/ad_zoidfs/ad_zoidfs_resize.c     \
    adio/ad_zoidfs/ad_zoidfs_features.c   \
    adio/ad_zoidfs/ad_zoidfs_read_list.c  \
    adio/ad_zoidfs/ad_zoidfs_write_list.c

endif BUILD_AD_ZOIDFS

