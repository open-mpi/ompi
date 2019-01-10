#
# Copyright (c) 2015-2018 Intel, Inc.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Only install the headers if we're in standalone mode

if WANT_PRIMARY_HEADERS
include_HEADERS = \
        pmix.h \
        pmix_server.h \
        pmix_tool.h \
	pmix_extend.h

if WANT_PMI_BACKWARD
include_HEADERS += \
        pmi.h \
        pmi2.h
endif

nodist_include_HEADERS = \
    pmix_common.h \
    pmix_version.h \
    pmix_rename.h

endif
