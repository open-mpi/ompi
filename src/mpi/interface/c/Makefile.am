# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# If the user selected --disable-profiling, then we don't traverse
# into the profile directory.

if COMPILE_PROFILING_SEPARATELY
profile_dir = profile
profile_lib = profile/libmpi_c_profile.la
else
profile_dir =
profile_lib =
endif

SUBDIRS = $(profile_dir)
DIST_SUBDIRS = profile

noinst_LTLIBRARIES = libmpi_c.la

headers = \
	bindings.h \
	bindings_profiling.h

libmpi_c_la_SOURCES = \
	comm_get_name.c \
	comm_set_name.c \
	init.c \
	finalize.c

libmpi_c_la_LIBADD	= $(profile_lib)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/c
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
