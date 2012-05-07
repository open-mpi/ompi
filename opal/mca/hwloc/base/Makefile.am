#
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = base/.windows

dist_pkgdata_DATA = base/help-opal-hwloc-base.txt

headers += \
        base/base.h

libmca_hwloc_la_SOURCES += \
        base/hwloc_base_close.c \
        base/hwloc_base_open.c

if OPAL_HAVE_HWLOC
libmca_hwloc_la_SOURCES += \
        base/hwloc_base_dt.c \
        base/hwloc_base_util.c \
        base/hwloc_base_maffinity.c
endif

