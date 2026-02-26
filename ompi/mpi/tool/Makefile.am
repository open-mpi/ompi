#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2018-2025 Triad National Security, LLC. All rights
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

include $(top_srcdir)/Makefile.ompi-rules

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

#
# the following must be processed by binding framework
# to generated both ompi and standard abi variants
#
prototype_sources = \
	cvar_get_info.c.in \
	cvar_handle_alloc.c.in \
	cvar_handle_free.c.in \
	cvar_read.c.in \
	cvar_write.c.in \
	enum_get_info.c.in \
	enum_get_item.c.in \
	event_callback_get_info.c.in \
	event_callback_set_info.c.in \
	event_copy.c.in \
	event_handle_get_info.c.in \
	event_handle_set_info.c.in \
	event_get_info.c.in \
	event_get_source.c.in \
	event_get_timestamp.c.in \
	event_handle_alloc.c.in \
	event_handle_free.c.in \
	event_read.c.in \
	event_register_callback.c.in \
	event_set_dropped_handler.c.in \
	pvar_get_info.c.in \
	pvar_handle_alloc.c.in \
	pvar_handle_free.c.in \
	pvar_read.c.in \
	pvar_readreset.c.in \
	pvar_reset.c.in \
	pvar_session_create.c.in \
	pvar_session_free.c.in \
	pvar_start.c.in \
	pvar_stop.c.in \
	pvar_write.c.in \
	source_get_info.c.in

#
# the following have function prototypes that work both for
# ompi and standard abi so do not requiring generating by the binding
# framework
#
interface_profile_sources_np = \
	category_changed.c \
	category_get_categories.c \
	category_get_cvars.c \
	category_get_events.c \
	category_get_info.c \
	category_get_index.c \
	category_get_num.c \
	category_get_num_events.c \
	category_get_pvars.c \
	cvar_get_index.c \
	cvar_get_num.c \
	event_get_index.c \
	event_get_num.c \
	finalize.c \
	init_thread.c \
	pvar_get_index.c \
	pvar_get_num.c \
	source_get_num.c \
	source_get_timestamp.c

interface_profile_sources = $(prototype_sources:.c.in=_ompi_generated.c) \
	$(interface_profile_sources_np)

if OMPI_GENERATE_BINDINGS
%_ompi_generated.c: %.c.in
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $@ \
	    c \
	    source \
	    ompi \
	    --mpit \
	    $<

%_abi_generated.c: %.c.in
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $@ \
	    c \
	    source \
	    standard \
	    --mpit \
	    $<
endif

libmpi_mpit_profile_la_SOURCES = $(interface_profile_sources)
libmpi_mpit_profile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

libmpi_mpit_noprofile_la_SOURCES = $(interface_profile_sources)
libmpi_mpit_noprofile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0

EXTRA_DIST = $(prototype_sources)

MAINTAINERCLEANFILES = *_generated.c 

if OMPI_STANDARD_ABI
include Makefile_abi.include
endif

