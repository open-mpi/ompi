## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_TESTFS

noinst_HEADERS += adio/ad_testfs/ad_testfs.h

romio_other_sources +=                \
    adio/ad_testfs/ad_testfs_close.c  \
    adio/ad_testfs/ad_testfs_read.c   \
    adio/ad_testfs/ad_testfs_rdcoll.c \
    adio/ad_testfs/ad_testfs_wrcoll.c \
    adio/ad_testfs/ad_testfs_open.c   \
    adio/ad_testfs/ad_testfs_write.c  \
    adio/ad_testfs/ad_testfs_done.c   \
    adio/ad_testfs/ad_testfs_fcntl.c  \
    adio/ad_testfs/ad_testfs_iread.c  \
    adio/ad_testfs/ad_testfs_iwrite.c \
    adio/ad_testfs/ad_testfs_wait.c   \
    adio/ad_testfs/ad_testfs_flush.c  \
    adio/ad_testfs/ad_testfs_seek.c   \
    adio/ad_testfs/ad_testfs_resize.c \
    adio/ad_testfs/ad_testfs_hints.c  \
    adio/ad_testfs/ad_testfs_delete.c \
    adio/ad_testfs/ad_testfs.c

endif BUILD_AD_TESTFS

