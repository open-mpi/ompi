#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = \
	include \
	$(MCA_orca_FRAMEWORKS_SUBDIRS) \
	$(MCA_orca_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
        . \
	$(MCA_orca_FRAMEWORK_COMPONENT_DSO_SUBDIRS)

DIST_SUBDIRS = \
	include \
	$(MCA_orca_FRAMEWORKS_SUBDIRS) \
	$(MCA_orca_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build the main ORCA library

lib_LTLIBRARIES = libopen-rca.la
libopen_rca_la_SOURCES =
libopen_rca_la_LIBADD = \
	$(MCA_orca_FRAMEWORK_LIBS) \
	$(top_ompi_builddir)/opal/libopen-pal-lt.la
#	$(top_ompi_builddir)/orte/libopen-rte-lt.la


libopen_rca_la_DEPENDENCIES = $(libopen_rca_la_LIBADD)
libopen_rca_la_LDFLAGS = -version-info $(libopen_rca_so_version)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
dist_pkgdata_DATA =
libopen_rca_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
orcadir = $(includedir)/openmpi/$(subdir)
nobase_orca_HEADERS = $(headers)
endif

# setup to catch extra files to be included in tarballs
EXTRA_DIST = CMakeLists.txt

include runtime/Makefile.am
include util/Makefile.am

# Set the convenience library to be the same as the non-convenience
# library, but a) it's marked as "noinst", so LT knows it's a
# convenience library, and b) no .so version number is assigned here.
# Note that we only need to make the convenience library if the OMPI
# project is being built (i.e., if there is an upper layer that needs
# to slurp in ORCA as a convenience library).

if PROJECT_ORCA
noinst_LTLIBRARIES += libopen-rca-lt.la
endif
libopen_rca_lt_la_SOURCES = $(libopen_rca_la_SOURCES)
libopen_rca_lt_la_LIBADD = $(libopen_rca_la_LIBADD)
libopen_rca_lt_la_DEPENDENCIES = $(libopen_rca_la_DEPENDENCIES)
