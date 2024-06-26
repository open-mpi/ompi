##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

if BUILD_AD_QUOBYTEFS

noinst_HEADERS += adio/ad_quobytefs/ad_quobytefs.h \
				  adio/ad_quobytefs/ad_quobytefs_internal.h

romio_other_sources +=        \
    adio/ad_quobytefs/ad_quobytefs.c      \
    adio/ad_quobytefs/ad_quobytefs_open.c  \
    adio/ad_quobytefs/ad_quobytefs_close.c  \
    adio/ad_quobytefs/ad_quobytefs_write.c  \
    adio/ad_quobytefs/ad_quobytefs_flush.c  \
    adio/ad_quobytefs/ad_quobytefs_fcntl.c  \
    adio/ad_quobytefs/ad_quobytefs_read.c  \
    adio/ad_quobytefs/ad_quobytefs_resize.c  \
    adio/ad_quobytefs/ad_quobytefs_delete.c  \
    adio/ad_quobytefs/ad_quobytefs_aio.c  \
    adio/ad_quobytefs/ad_quobytefs_setlock.c \
    adio/ad_quobytefs/ad_quobytefs_internal.c

endif BUILD_AD_QUOBYTEFS
