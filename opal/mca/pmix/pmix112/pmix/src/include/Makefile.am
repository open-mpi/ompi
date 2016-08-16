#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2016 Intel, Inc. All rights reserved
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from
# src/Makefile.am

headers += \
        src/include/pmix_globals.h

sources += \
        src/include/pmix_globals.c

if ! PMIX_EMBEDDED_MODE
headers += \
        src/include/pmix_config.h \
        src/include/align.h \
        src/include/hash_string.h \
        src/include/pmix_socket_errno.h \
        src/include/prefetch.h \
        src/include/types.h

endif ! PMIX_EMBEDDED_MODE
