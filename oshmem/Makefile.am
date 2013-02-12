#
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if OSHMEM_SUPPORT

AM_CFLAGS = $(OSHMEM_CFLAGS)

EXTRA_DIST = CMakeLists.txt

# Do we have profiling?
if OSHMEM_PROFILING
c_pshmem_lib = shmem/c/profile/libshmem_c_pshmem.la
else
c_pshmem_lib = 
endif

#Irit
f77_shmem_lib = shmem/f77/libshmem_f77.la

# Note that the ordering of "." in SUBDIRS is important: the C++, F77,
# and F90 bindings are all in standalone .la files that depend on
# libshmem.la.  So we must fully build libshmem.la first.

# NOTE: A handful of files in mpi/f77/base must be included in
# libshmem.la.  But we wanted to keep all the Fortran sources together
# in the same tree, so we moved those sources to a separate
# subdirectory with its own Makefile.include that is included in this
# Makefile.am (NOTE: it did *not* work to put all the files -- base
# q
# and non-base -- into mpi/f77 and have both a regular Makefile.am for
# building the f77 bindings library and a separate Makefile.include
# that was included in this top-level Makefile.am; problems occurred
# with "make distclean" and files in the ompi/mpi/f77/.deps directory
# -- it's not clear whether this is an AM bug or whether this behavior
# is simply not supported).  This ompi/mpi/f77/base/Makefile.include
# file makes a convenience LT library that is then sucked into
# libshmem.la (the ompi/mpi/f77/base sources must be compiled with
# special CPPFLAGS; we can't just add the raw sources to
# libshmem_la_SOURCES, unfortunately).

# The end of the result is that libshmem.la -- including a few sources
# from mpi/f77/base -- is fully built before the C++, F77, and F90
# bindings are built.  Therefore, the C++, F77 and F90 bindings
# libraries can all link against libshmem.la.

SUBDIRS = \
	include \
	shmem/c \
	shmem/f77 \
	$(EXT_oshmem_FRAMEWORKS_SUBDIRS) \
	$(EXT_oshmem_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
	$(MCA_oshmem_FRAMEWORKS_SUBDIRS) \
	$(MCA_oshmem_FRAMEWORK_COMPONENT_STATIC_SUBDIRS) \
	. \
	$(MCA_oshmem_FRAMEWORK_COMPONENT_DSO_SUBDIRS) 
    
DIST_SUBDIRS = \
	include \
	shmem/c \
	shmem/f77 \
	$(EXT_oshmem_FRAMEWORKS_SUBDIRS) \
	$(EXT_oshmem_FRAMEWORK_COMPONENT_ALL_SUBDIRS) \
	$(MCA_oshmem_FRAMEWORKS_SUBDIRS) \
	$(MCA_oshmem_FRAMEWORK_COMPONENT_ALL_SUBDIRS) 

#Build The main SHMEM library 
lib_LTLIBRARIES = libshmem.la
libshmem_la_SOURCES =
libshmem_la_LIBADD = \
	shmem/c/libshmem_c.la \
	$(c_pshmem_lib) \
	$(f77_shmem_lib) \
	$(MCA_oshmem_FRAMEWORK_LIBS) \
	$(top_ompi_builddir)/ompi/libmpi.la
libshmem_la_DEPENDENCIES = $(libshmem_la_LIBADD)
libshmem_la_LDFLAGS = \
        -version-info $(libshmem_so_version) \
        $(OSHMEM_LIBSHMEM_EXTRA_LDFLAGS)

# included subdirectory Makefile.am's and appended-to variables
headers =
noinst_LTLIBRARIES =
include_HEADERS =
nobase_oshmem_HEADERS = 
dist_pkgdata_DATA = 
libshmem_la_SOURCES += $(headers)
nodist_man_MANS =

# Conditionally install the header files

if WANT_INSTALL_HEADERS
oshmemdir = $(includedir)/oshmem/oshmem
nobase_oshmem_HEADERS += $(headers)
else
oshmemdir = $(includedir)
endif

include op/Makefile.am
include proc/Makefile.am
include request/Makefile.am
include runtime/Makefile.am
include shmem/Makefile.am
include tools/Makefile.am

# Ensure that the man page directory exists before we try to make man
# page files (because oshmem/shmem/man/man3 has no config.status-generated
# Makefile)
dir_stamp = $(top_builddir)/$(subdir)/shmem/man/man3/.dir-stamp

# Also ensure that the man pages are rebuilt if the opal_config.h file
# changes (e.g., configure was run again, meaning that the release
# date or version may have changed)
$(nodist_man_MANS): $(dir_stamp) $(top_builddir)/opal/include/opal_config.h

$(dir_stamp):
	$(mkdir_p) `dirname $@`
	touch "$@"

# Remove the generated man pages
distclean-local:
	rm -f $(nodist_man_MANS) $(dir_stamp)

endif # OSHMEM_SUPPORT
