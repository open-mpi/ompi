#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
	-DOMPI_SYSCONFDIR="\"$(sysconfdir)\""

noinst_LTLIBRARIES = libmca_llm_hostfile.la
libmca_llm_hostfile_la_SOURCES = \
	llm_hostfile.h \
	llm_hostfile_component.c \
	llm_hostfile_allocate.c \
	llm_hostfile_deallocate.c
