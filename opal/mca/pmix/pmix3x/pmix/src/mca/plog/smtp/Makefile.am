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
# Copyright (c) 2009-2010 Cisco Systems, Inc. All rights reserved.
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(plog_smtp_CPPFLAGS)

dist_pmixdata_DATA = \
        help-pmix-plog-smtp.txt

sources = \
        plog_smtp.h \
        plog_smtp.c \
        plog_smtp_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_plog_smtp_DSO
component_noinst =
component_install = mca_plog_smtp.la
else
component_noinst = libmca_plog_smtp.la
component_install =
endif

mcacomponentdir = $(pmnixlibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_plog_smtp_la_SOURCES = $(sources)
mca_plog_smtp_la_LDFLAGS = -module -avoid-version $(plog_smtp_LDFLAGS)
mca_plog_smtp_la_LIBADD = $(plog_smtp_LIBS) $(top_builddir)/src/libpmix.la
if NEED_LIBPMIX
mca_plog_smtp_la_LIBADD = $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(component_noinst)
libmca_plog_smtp_la_SOURCES =$(sources)
libmca_plog_smtp_la_LDFLAGS = -module -avoid-version $(plog_smtp_LDFLAGS)
libmca_plog_smtp_la_LIBADD = $(plog_smtp_LIBS)
