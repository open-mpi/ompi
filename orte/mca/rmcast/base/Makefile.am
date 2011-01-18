#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_pkgdata_DATA += base/help-rmcast-base.txt

headers += \
        base/base.h \
        base/private.h

libmca_rmcast_la_SOURCES += \
        base/rmcast_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

libmca_rmcast_la_SOURCES += \
        base/rmcast_base_close.c \
	base/rmcast_base_select.c \
	base/rmcast_base_fns.c \
        base/rmcast_base_threads.c

endif
