#
# Copyright (c)      2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers += \
        base/base.h

libmca_sstore_la_SOURCES += \
        base/sstore_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

dist_pkgdata_DATA = base/help-orte-sstore-base.txt

libmca_sstore_la_SOURCES += \
        base/sstore_base_close.c \
        base/sstore_base_select.c \
        base/sstore_base_fns.c
endif
