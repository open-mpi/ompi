#
# Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers = \
	mpiext_cr_c.h

sources = \
	c/checkpoint.c \
	c/restart.c \
	c/migrate.c \
	c/inc_register_callback.c \
	c/quiesce_start.c \
	c/quiesce_end.c \
	c/quiesce_checkpoint.c \
	c/self_register_checkpoint.c \
	c/self_register_restart.c \
	c/self_register_continue.c

lib = libext_mpiext_cr.la
lib_sources = $(sources)

extcomponentdir = $(pkglibdir)

noinst_LTLIBRARIES = $(lib)
libext_mpiext_cr_la_SOURCES = $(lib_sources)
libext_mpiext_cr_la_LDFLAGS = -module -avoid-version

ompidir = $(includedir)/openmpi/ompi/mpiext/cr
ompi_HEADERS = \
        $(headers)
