#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA += base/help-pmix-base.txt

headers += \
	base/base.h \
	base/pmix_base_fns.h \
        base/pmix_base_hash.h

libmca_pmix_la_SOURCES += \
	base/pmix_base_frame.c \
	base/pmix_base_select.c \
	base/pmix_base_fns.c \
        base/pmix_base_hash.c

if OPAL_PMIX2_COMMON_REQD

AM_CPPFLAGS += @OPAL_PMIX2_COMMON_CPPFLAGS@

dist_opaldata_DATA += base/common/help-pmix2x.txt

headers += base/common/pmix2x_common.h

libmca_pmix_la_SOURCES += \
	base/common/pmix2x.c \
	base/common/pmix2x_client.c \
	base/common/pmix2x_server_north.c \
	base/common/pmix2x_server_south.c
endif
