# -*- makefile -*-
#
# Copyright (c) 2001-2004 The Trustees of Indiana University.  
#                         All rights reserved.
# Copyright (c) 1998-2001 University of Notre Dame. 
#                         All rights reserved.
# Copyright (c) 1994-1998 The Ohio State University.  
#                         All rights reserved.
# 
# This file is part of the LAM/MPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# LAM/MPI source distribution.
#
# See APPLE_LICENSE file included in this directory for additional notices
# about the original ptmalloc package.
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS		= \
			-I$(top_srcdir)/src \
			-I$(top_lam_builddir)/share/include \
			-I$(top_lam_srcdir)/share/include

# Apple requires us to include the copyright notice in the software.
# So install it in the same place that we install ROMIO's copyright
# notices.
docdir			= $(datadir)/lam/doc
doc_DATA		= APPLE_LICENSE

if LAM_WANT_DARWIN7MALLOC
noinst_LTLIBRARIES	= libdarwin7malloc.la
endif

libdarwin7malloc_la_SOURCES = \
			pthread_machdep.h \
			pthread_spinlock.h \
			scalable_malloc.c \
			scalable_malloc.h

EXTRA_DIST		= \
			$(doc_DATA)
