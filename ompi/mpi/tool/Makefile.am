#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2012-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# The purpose of the profiling layer is to allow intercept libraries
# which override the MPI_ namespace symbols.  We potentially compile
# every MPI function twice.  We always build the profiling layer,
# because the symbols that are always implemented as functions are the
# PMPI_ namespace symbols.  We sometimes also build the non-profiling
# layer, if weak symbols can't be used to alias the MPI_ namespace
# into the PMPI_ namespace.

noinst_LTLIBRARIES = libmpi_mpit.la libmpi_mpit_profile.la
if BUILD_MPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_mpit_noprofile.la
endif

headers = mpit-internal.h

# mpit_common.c is not public functions, which does not have profiling
# implications, so they are always built.
libmpi_mpit_la_SOURCES = \
	mpit_common.c
libmpi_mpit_la_LIBADD = libmpi_mpit_profile.la
if BUILD_MPI_BINDINGS_LAYER
libmpi_mpit_la_LIBADD += libmpi_mpit_noprofile.la
endif

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
ompi_HEADERS = $(headers)
endif

#
# List of all C files that have profile versions
#
interface_profile_sources = \
	category_changed.c \
	category_get_categories.c \
	category_get_cvars.c \
	category_get_info.c \
	category_get_index.c \
	category_get_num.c \
	category_get_pvars.c \
	cvar_get_info.c \
	cvar_get_index.c \
	cvar_get_num.c \
	cvar_handle_alloc.c \
	cvar_handle_free.c \
	cvar_read.c \
	cvar_write.c \
	enum_get_info.c \
	enum_get_item.c \
	finalize.c \
	init_thread.c \
	pvar_get_info.c \
	pvar_get_index.c \
	pvar_get_num.c \
	pvar_handle_alloc.c \
	pvar_handle_free.c \
	pvar_read.c \
	pvar_readreset.c \
	pvar_reset.c \
	pvar_session_create.c \
	pvar_session_free.c \
	pvar_start.c \
	pvar_stop.c \
	pvar_write.c

libmpi_mpit_profile_la_SOURCES = $(interface_profile_sources)
libmpi_mpit_profile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

libmpi_mpit_noprofile_la_SOURCES = $(interface_profile_sources)
libmpi_mpit_noprofile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0
