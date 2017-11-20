## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_PFS

noinst_HEADERS += adio/ad_pfs/ad_pfs.h

romio_other_sources +=          \
    adio/ad_pfs/ad_pfs_read.c   \
    adio/ad_pfs/ad_pfs_open.c   \
    adio/ad_pfs/ad_pfs_write.c  \
    adio/ad_pfs/ad_pfs_done.c   \
    adio/ad_pfs/ad_pfs_fcntl.c  \
    adio/ad_pfs/ad_pfs_iread.c  \
    adio/ad_pfs/ad_pfs_iwrite.c \
    adio/ad_pfs/ad_pfs_wait.c   \
    adio/ad_pfs/ad_pfs_flush.c  \
    adio/ad_pfs/ad_pfs_hints.c  \
    adio/ad_pfs/ad_pfs.c

endif BUILD_AD_PFS

