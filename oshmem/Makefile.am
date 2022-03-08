#
# Copyright (c) 2013-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Do we have the Fortran bindings?
if OSHMEM_BUILD_FORTRAN_BINDINGS
fortran_oshmem_lib = shmem/fortran/liboshmem_fortran.la
else
fortran_oshmem_lib =
endif

SUBDIRS = \
	include \
	shmem/c \
	shmem/fortran

if PROJECT_OSHMEM
# Only traverse these dirs if we're building oshmem
SUBDIRS += \
	$(MCA_oshmem_FRAMEWORKS_SUBDIRS) \
	$(MCA_oshmem_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
	. \
	$(MCA_oshmem_FRAMEWORK_COMPONENT_DSO_SUBDIRS)
endif

DIST_SUBDIRS = \
	include \
	shmem/c \
	shmem/fortran \
	$(MCA_oshmem_FRAMEWORKS_SUBDIRS) \
	$(MCA_oshmem_FRAMEWORK_COMPONENT_ALL_SUBDIRS)

# Build The main OSHMEM library, but only if we're building OSHMEM
lib_LTLIBRARIES =
if PROJECT_OSHMEM
lib_LTLIBRARIES += liboshmem.la
endif

liboshmem_la_SOURCES =
liboshmem_la_LIBADD = \
	shmem/c/liboshmem_c.la \
	$(fortran_oshmem_lib) \
	$(MCA_oshmem_FRAMEWORK_LIBS) \
	$(OSHMEM_TOP_BUILDDIR)/ompi/lib@OMPI_LIBMPI_NAME@.la
liboshmem_la_DEPENDENCIES = $(liboshmem_la_LIBADD)
liboshmem_la_LDFLAGS = \
        -version-info $(liboshmem_so_version) \
        $(OSHMEM_LIBSHMEM_EXTRA_LDFLAGS)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
include_HEADERS =
dist_oshmemdata_DATA =
liboshmem_la_SOURCES += $(headers)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
oshmemdir = $(oshmemincludedir)/$(subdir)
nobase_oshmem_HEADERS = $(headers)
endif

include op/Makefile.am
include proc/Makefile.am
include request/Makefile.am
include info/Makefile.am
include runtime/Makefile.am
include shmem/Makefile.am
include mca/Makefile.am
include tools/Makefile.am
include util/Makefile.am
