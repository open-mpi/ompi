#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if WANT_PERUSE
check_PROGRAMS = mpi_peruse
else
check_PROGRAMS =
endif

TESTS = \
	$(check_PROGRAMS)

mpi_peruse_SOURCES = mpi_peruse.c
mpi_peruse_LDFLAGS = $(WRAPPER_EXTRA_LDFLAGS)
mpi_peruse_LDADD = \
        $(top_builddir)/ompi/libmpi.la
mpi_peruse_DEPENDENCIES = $(mpi_peruse_LDADD)

