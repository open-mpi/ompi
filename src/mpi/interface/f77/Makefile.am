# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# We must traverse into this directory to install the proper header
# files.  So only build the f77 bindings if we want to build them, and
# only expect to find the f77 profiling library if we're building the
# f77 bindings *and* we're compiling profiling separately.

# If the user selected --disable-profiling, then we don't traverse
# into the profile directory.

if BUILD_MPI_F77
f77_lib = libmpi_f77.la

if COMPILE_PROFILING_SEPARATELY
profile_lib = profile/libmpi_f77_profile.la
else
profile_lib =
endif

else
f77_lib =
profile_lib =
endif

SUBDIRS = profile

noinst_LTLIBRARIES = $(f77_lib)

headers = \
	bindings.h \
	prototypes.h

libmpi_f77_la_SOURCES = \
        alloc_mem_f.c \
	comm_get_name_f.c \
	comm_set_name_f.c \
	init_f.c \
	finalize_f.c \
        free_mem_f.c

libmpi_f77_la_LIBADD	= $(profile_lib)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/f77
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
