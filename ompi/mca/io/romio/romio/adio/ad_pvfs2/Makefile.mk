## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_PVFS2

noinst_HEADERS += adio/ad_pvfs2/ad_pvfs2.h \
	adio/ad_pvfs2/ad_pvfs2_io.h \
	adio/ad_pvfs2/ad_pvfs2_common.h

romio_other_sources +=                          \
    adio/ad_pvfs2/ad_pvfs2_close.c              \
    adio/ad_pvfs2/ad_pvfs2_read.c               \
    adio/ad_pvfs2/ad_pvfs2_open.c               \
    adio/ad_pvfs2/ad_pvfs2_write.c              \
    adio/ad_pvfs2/ad_pvfs2_fcntl.c              \
    adio/ad_pvfs2/ad_pvfs2_flush.c              \
    adio/ad_pvfs2/ad_pvfs2_resize.c             \
    adio/ad_pvfs2/ad_pvfs2_hints.c              \
    adio/ad_pvfs2/ad_pvfs2_delete.c             \
    adio/ad_pvfs2/ad_pvfs2.c                    \
    adio/ad_pvfs2/ad_pvfs2_common.c             \
    adio/ad_pvfs2/ad_pvfs2_aio.c                \
    adio/ad_pvfs2/ad_pvfs2_read_list_classic.c  \
    adio/ad_pvfs2/ad_pvfs2_io_list.c            \
    adio/ad_pvfs2/ad_pvfs2_io_dtype.c           \
    adio/ad_pvfs2/ad_pvfs2_write_list_classic.c \
    adio/ad_pvfs2/ad_pvfs2_features.c

endif BUILD_AD_PVFS2

