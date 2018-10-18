## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_PANFS_OPEN6
# override open with one that uses newer features
panfs_open = adio/ad_panfs/ad_panfs_open6.c
else
panfs_open = adio/ad_panfs/ad_panfs_open.c
endif

if BUILD_AD_PANFS

noinst_HEADERS += adio/ad_panfs/ad_panfs.h

romio_other_sources +=              \
    adio/ad_panfs/ad_panfs.c        \
    $(panfs_open)                   \
    adio/ad_panfs/ad_panfs_hints.c  \
    adio/ad_panfs/ad_panfs_read.c   \
    adio/ad_panfs/ad_panfs_resize.c \
    adio/ad_panfs/ad_panfs_write.c

endif BUILD_AD_PANFS

