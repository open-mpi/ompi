##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

if BUILD_AD_DAOS

noinst_HEADERS += adio/ad_daos/ad_daos.h

romio_other_sources += \
    adio/ad_daos/ad_daos.c \
    adio/ad_daos/ad_daos_close.c \
    adio/ad_daos/ad_daos_common.c \
    adio/ad_daos/ad_daos_fcntl.c \
    adio/ad_daos/ad_daos_features.c \
    adio/ad_daos/ad_daos_hhash.c \
    adio/ad_daos/ad_daos_hints.c \
    adio/ad_daos/ad_daos_io.c \
    adio/ad_daos/ad_daos_io_str.c \
    adio/ad_daos/ad_daos_open.c \
    adio/ad_daos/ad_daos_resize.c

endif BUILD_AD_DAOS

