## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2011 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_GRIDFTP

noinst_HEADERS += adio/ad_gridftp/ad_gridftp.h

romio_other_sources +=                    \
    adio/ad_gridftp/ad_gridftp_close.c    \
    adio/ad_gridftp/ad_gridftp_open.c     \
    adio/ad_gridftp/ad_gridftp_read.c     \
    adio/ad_gridftp/ad_gridftp_write.c    \
    adio/ad_gridftp/ad_gridftp_fcntl.c    \
    adio/ad_gridftp/ad_gridftp_flush.c    \
    adio/ad_gridftp/ad_gridftp_resize.c   \
    adio/ad_gridftp/ad_gridftp_hints.c    \
    adio/ad_gridftp/ad_gridftp_delete.c   \
    adio/ad_gridftp/ad_gridftp.c          \
    adio/ad_gridftp/globus_routines.c     \
    adio/ad_gridftp/ad_gridftp_features.c

endif BUILD_AD_GRIDFTP

