# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# We must traverse into the profile directory to install the proper
# header files.  So only only expect to find the C profiling library
# if we're compiling profiling separately.

if COMPILE_PROFILING_SEPARATELY
profile_lib = profile/libmpi_c_profile.la
else
profile_lib =
endif

SUBDIRS = profile

noinst_LTLIBRARIES = libmpi_c.la

headers = bindings.h

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
