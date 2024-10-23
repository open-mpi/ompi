##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

if BUILD_AD_LUSTRE

noinst_HEADERS += adio/ad_lustre/ad_lustre.h

romio_other_sources +=                   \
    adio/ad_lustre/ad_lustre.c           \
    adio/ad_lustre/ad_lustre_open.c      \
    adio/ad_lustre/ad_lustre_rwcontig.c  \
    adio/ad_lustre/ad_lustre_wrcoll.c    \
    adio/ad_lustre/ad_lustre_wrstr.c     \
    adio/ad_lustre/ad_lustre_hints.c     \
    adio/ad_lustre/ad_lustre_aggregate.c

if LUSTRE_LOCKAHEAD
romio_other_sources +=                   \
    adio/ad_lustre/ad_lustre_lock.c
endif LUSTRE_LOCKAHEAD

endif BUILD_AD_LUSTRE
