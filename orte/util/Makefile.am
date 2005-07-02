#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DOMPI_PKGDATADIR=\"$(pkgdatadir)\"

noinst_LTLIBRARIES = liborteutil.la

# Source code files

headers = \
        proc_info.h \
        session_dir.h \
        universe_setup_file_io.h \
        univ_info.h

liborteutil_la_SOURCES = \
        $(headers) \
        proc_info.c \
        session_dir.c \
        universe_setup_file_io.c \
        univ_info.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/util
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif
