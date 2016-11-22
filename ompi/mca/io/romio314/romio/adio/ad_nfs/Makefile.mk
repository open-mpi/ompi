## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_NFS

noinst_HEADERS += adio/ad_nfs/ad_nfs.h

romio_other_sources +=            \
    adio/ad_nfs/ad_nfs_read.c     \
    adio/ad_nfs/ad_nfs_open.c     \
    adio/ad_nfs/ad_nfs_write.c    \
    adio/ad_nfs/ad_nfs_done.c     \
    adio/ad_nfs/ad_nfs_fcntl.c    \
    adio/ad_nfs/ad_nfs_iread.c    \
    adio/ad_nfs/ad_nfs_iwrite.c   \
    adio/ad_nfs/ad_nfs_wait.c     \
    adio/ad_nfs/ad_nfs_setsh.c    \
    adio/ad_nfs/ad_nfs_getsh.c    \
    adio/ad_nfs/ad_nfs.c          \
    adio/ad_nfs/ad_nfs_resize.c   \
    adio/ad_nfs/ad_nfs_features.c

endif BUILD_AD_NFS

