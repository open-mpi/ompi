#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_opaldata_DATA += base/help-pmi-base.txt

headers += \
	base/base.h

libmca_pmi_la_SOURCES += \
	base/pmi_base_frame.c \
	base/pmi_base_select.c

if WANT_PMI_SUPPORT
        base/pmi_base_fns.c
endif
