#
# Copyright 2015-2016 Intel, Inc. All rights reserved
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
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
        include/pmix.h \
        include/pmix_server.h \
        include/pmi.h \
        include/pmi2.h \
        include/pmix_version.h.in

include_pmixdir = $(includedir)/pmix
include_pmix_HEADERS = \
    include/pmix/pmix_common.h

include_pmix_autogendir = $(includedir)/pmix/autogen
include_pmix_autogen_HEADERS = \
        include/pmix/autogen/pmix_config_top.h \
        include/pmix/autogen/pmix_config_bottom.h \
        include/pmix/autogen/pmix_stdint.h
nodist_include_pmix_autogen_HEADERS = \
        include/pmix/autogen/config.h

nodist_include_HEADERS = \
    include/pmix_version.h

endif ! PMIX_EMBEDDED_MODE
