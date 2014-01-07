#
# Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file builds the C bindings for MPI extensions.  It must be
# present in all MPI extensions.

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libmpiext_cr_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/ompi/mpiext/cr/c

# This is the header file that is installed.
ompi_HEADERS = mpiext_cr_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
libmpiext_cr_c_la_SOURCES = \
        $(ompi_HEADERS) \
	checkpoint.c \
	restart.c \
	migrate.c \
	inc_register_callback.c \
	quiesce_start.c \
	quiesce_end.c \
	quiesce_checkpoint.c \
	self_register_checkpoint.c \
	self_register_restart.c \
	self_register_continue.c
libmpiext_cr_c_la_LDFLAGS = -module -avoid-version
