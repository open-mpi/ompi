## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_PIOFS

noinst_HEADERS += adio/ad_piofs/ad_piofs.h

romio_other_sources +=             \
    adio/ad_piofs/ad_piofs_read.c  \
    adio/ad_piofs/ad_piofs_open.c  \
    adio/ad_piofs/ad_piofs_write.c \
    adio/ad_piofs/ad_piofs_fcntl.c \
    adio/ad_piofs/ad_piofs_hints.c \
    adio/ad_piofs/ad_piofs.c

endif BUILD_AD_PIOFS

