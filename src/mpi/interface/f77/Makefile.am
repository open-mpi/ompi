# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# If the user selected --disable-profiling, then we don't traverse
# into the profile directory.

if COMPILE_PROFILING_SEPARATELY
profile_dir = profile
profile_lib = profile/libmpi_f77_profile.la
else
profile_dir =
profile_lib =
endif

SUBDIRS = $(profile_dir)
DIST_SUBDIRS = profile

noinst_LTLIBRARIES = libmpi_f77.la

headers = \
	bindings.h \
	prototypes.h

libmpi_f77_la_SOURCES = \
	comm_get_name_f.c \
	comm_set_name_f.c \
	init_f.c \
	finalize_f.c

libmpi_f77_la_LIBADD	= $(profile_lib)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/f77
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
