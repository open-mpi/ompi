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
        alloc_mem.c \
	comm_get_name.c \
	comm_set_name.c \
	init.c \
	finalize.c \
        free_mem.c \
	type_commit.c \
	type_contiguous.c \
	type_create_darray.c \
	type_create_f90_complex.c \
	type_create_f90_integer.c \
	type_create_f90_real.c \
	type_create_hindexed.c \
	type_create_hvector.c \
	type_create_indexed_block.c \
	type_create_keyval.c \
	type_create_resized.c \
	type_create_struct.c \
	type_create_subarray.c \
	type_delete_attr.c \
	type_dup.c \
	type_extent.c \
	type_free.c \
	type_free_keyval.c \
	type_get_attr.c \
	type_get_contents.c \
	type_get_envelope.c \
	type_get_extent.c \
	type_get_name.c \
	type_get_true_extent.c \
	type_hindexed.c \
	type_hvector.c \
	type_indexed.c \
	type_lb.c \
	type_match_size.c \
	type_set_attr.c \
	type_set_name.c \
	type_size.c \
	type_struct.c \
	type_ub.c \
	type_vector.c

libmpi_c_la_LIBADD	= $(profile_lib)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/c
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
