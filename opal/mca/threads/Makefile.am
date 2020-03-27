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
# Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# main library setup
noinst_LTLIBRARIES = libmca_threads.la
libmca_threads_la_SOURCES =

# local files
headers = \
        condition.h \
        mutex.h \
        thread.h \
        threads.h \
        thread_usage.h \
        tsd.h \
        wait_sync.h
nodist_headers =
libmca_threads_la_SOURCES += $(headers)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
nobase_opal_HEADERS = $(headers)
nobase_nodist_opal_HEADERS = $(nodist_headers)
endif

include base/Makefile.am

distclean-local:
	rm -f base/static-components.h
