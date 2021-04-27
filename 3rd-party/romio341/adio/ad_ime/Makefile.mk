##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

if BUILD_AD_IME

noinst_HEADERS += adio/ad_ime/ad_ime.h adio/ad_ime/ad_ime_common.h

romio_other_sources +=                    \
    adio/ad_ime/ad_ime.c            \
    adio/ad_ime/ad_ime_close.c      \
    adio/ad_ime/ad_ime_common.c     \
    adio/ad_ime/ad_ime_delete.c     \
    adio/ad_ime/ad_ime_fcntl.c      \
    adio/ad_ime/ad_ime_flush.c      \
    adio/ad_ime/ad_ime_io.c         \
    adio/ad_ime/ad_ime_open.c       \
    adio/ad_ime/ad_ime_resize.c     \
    adio/ad_ime/ad_ime_features.c

endif BUILD_AD_IME
