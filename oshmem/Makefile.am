#
# Copyright (c) 2013-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Do we have profiling?
if OSHMEM_PROFILING
c_pshmem_lib = shmem/c/profile/liboshmem_c_pshmem.la
else
c_pshmem_lib =
endif

# Do we have the Fortran bindings?
if OSHMEM_BUILD_FORTRAN_BINDINGS
fortran_oshmem_lib = shmem/fortran/liboshmem_fortran.la

if OSHMEM_PROFILING
fortran_pshmem_lib = shmem/fortran/profile/liboshmem_fortran_pshmem.la
endif

else
fortran_oshmem_lib =
fortran_pshmem_lib =
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
	$(c_pshmem_lib) \
	$(fortran_oshmem_lib) \
	$(fortran_pshmem_lib) \
	$(MCA_oshmem_FRAMEWORK_LIBS) \
	$(OSHMEM_TOP_BUILDDIR)/ompi/libmpi.la
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
nodist_man_MANS =

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
include shmem/man/man3/Makefile.extra
include mca/Makefile.am
include tools/Makefile.am
include util/Makefile.am

# Ensure that the man page directory exists before we try to make man
# page files (because ompi/mpi/man/man3 has no config.status-generated
# Makefile)
dir_stamp = $(top_builddir)/$(subdir)/shmem/man/man3/.dir-stamp

# Also ensure that the man pages are rebuilt if the opal_config.h file
# changes (e.g., configure was run again, meaning that the release
# date or version may have changed)
$(nodist_man_MANS): $(dir_stamp) $(top_builddir)/opal/include/opal_config.h

$(dir_stamp):
	$(MKDIR_P) `dirname $@`
	touch "$@"

# Remove the generated man pages
distclean-local:
	rm -f $(nodist_man_MANS) $(dir_stamp)
