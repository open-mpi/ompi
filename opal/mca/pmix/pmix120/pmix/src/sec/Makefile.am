#
# Copyright (c) 2015      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers += \
        src/sec/pmix_sec.h \
        src/sec/pmix_native.h

sources += \
        src/sec/pmix_sec.c \
        src/sec/pmix_native.c

if PMIX_WANT_MUNGE
headers += \
        src/sec/pmix_munge.h

sources += \
        src/sec/pmix_munge.c
endif

if PMIX_WANT_SASL
headers += \
        src/sec/pmix_sasl.h

sources += \
        src/sec/pmix_sasl.c
endif
