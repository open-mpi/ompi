#
# Copyright 2015-2016 Intel, Inc. All rights reserved
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Only install the headers if we're in standalone mode

if ! PMIX_EMBEDDED_MODE
include_HEADERS = \
        pmix.h \
        pmix_common.h \
        pmix_server.h \
        pmi.h \
        pmi2.h \
        pmix_tool.h

nodist_include_HEADERS = \
    pmix_version.h \
    pmix_rename.h

endif ! PMIX_EMBEDDED_MODE
