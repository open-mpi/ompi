#
# Copyright (c) 2015      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers += \
        sec/pmix_sec.h \
        sec/pmix_native.h

sources += \
        sec/pmix_sec.c \
        sec/pmix_native.c

if PMIX_WANT_MUNGE
headers += \
        sec/pmix_munge.h

sources += \
        sec/pmix_munge.c
endif

if PMIX_WANT_SASL
headers += \
        sec/pmix_sasl.h

sources += \
        sec/pmix_sasl.c
endif
