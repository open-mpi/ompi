#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2018-2019 Triad National Security, LLC. All rightsa
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

SUBDIRS = profile

AM_CPPFLAGS = -DOMPI_PROFILING_DEFINES=0

noinst_LTLIBRARIES = libmpi_mpit_common.la

if BUILD_MPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_mpit.la
endif

headers = mpit-internal.h

libmpi_mpit_common_la_SOURCES = mpit_common.c

libmpi_mpit_la_SOURCES = init_thread.c finalize.c cvar_get_num.c \
		     cvar_get_info.c cvar_read.c cvar_write.c \
                     cvar_handle_alloc.c cvar_handle_free.c \
                     category_get_num.c category_get_info.c \
                     category_get_cvars.c category_get_pvars.c \
                     category_get_categories.c category_changed.c \
                     pvar_get_info.c pvar_get_num.c pvar_handle_alloc.c \
                     pvar_handle_free.c pvar_read.c pvar_readreset.c \
                     pvar_reset.c pvar_session_create.c pvar_session_free.c \
                     pvar_start.c pvar_stop.c pvar_write.c \
                     enum_get_info.c enum_get_item.c cvar_get_index.c \
                     pvar_get_index.c category_get_index.c \
                     event_get_index.c event_copy.c event_callback_get_info.c \
                     event_callback_set_info.c event_handle_get_info.c \
                     event_handle_set_info.c event_get_info.c event_get_num.c event_get_source.c \
                     event_get_timestamp.c event_handle_alloc.c event_handle_free.c \
                     event_read.c event_register_callback.c event_set_dropped_handler.c \
                     source_get_num.c source_get_info.c source_get_timestamp.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
ompi_HEADERS = $(headers)
endif
