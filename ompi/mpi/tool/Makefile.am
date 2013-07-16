#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = profile

AM_CPPFLAGS = -DOMPI_PROFILING_DEFINES=0

noinst_LTLIBRARIES = libmpi_mpit.la

headers = mpit-internal.h

libmpi_mpit_la_SOURCES = init_thread.c finalize.c cvar_get_num.c \
		     cvar_get_info.c cvar_read.c cvar_write.c \
                     cvar_handle_alloc.c cvar_handle_free.c \
                     catagory_get_num.c catagory_get_info.c \
                     catagory_get_cvars.c catagory_get_pvars.c \
                     catagory_get_catagories.c catagory_changed.c \
                     pvar_get_info.c pvar_get_num.c pvar_handle_alloc.c \
                     pvar_handle_free.c pvar_read.c pvar_readreset.c \
                     pvar_reset.c pvar_session_create.c pvar_session_free.c \
                     pvar_start.c pvar_stop.c pvar_write.c \
                     enum_get_info.c enum_get_item.c mpit_common.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/$(subdir)
ompi_HEADERS = $(headers)
endif
