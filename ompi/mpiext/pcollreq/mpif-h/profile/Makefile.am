#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This file builds the mpif.h-based bindings for MPI extensions.  It
# is optional in MPI extensions.

# We must set these #defines so that the inner OMPI MPI prototype
# header files do the Right Thing.
AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

# Note that the mpif.h-based bindings are optional -- they can only be
# built if OMPI is also building the Fortran-based bindings.  So we
# initially set some Makefile macros to empty, and then conditionally
# add to them later.
noinst_LTLIBRARIES =

# Directory where the header file to be included in mpif-ext.h must be
# installed.
ompidir = $(ompiincludedir)/ompi/mpiext/pcollreq/mpif-h

# Just like noinst_LTLIBRARIES, set this macro to empty and
# conditionally add to it later.
ompi_HEADERS =

# Use the Automake conditional to know if we're building the mpif.h
# bindings.
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS

# If we are, build the convenience libtool library that will be
# slurped up into libmpi_mpifh.la.
noinst_LTLIBRARIES += libpmpiext_pcollreq_mpifh.la

# Sources for the convenience libtool library.
nodist_libpmpiext_pcollreq_mpifh_la_SOURCES = \
        pallgather_init_f.c \
        pallgatherv_init_f.c \
        pallreduce_init_f.c \
        palltoall_init_f.c \
        palltoallv_init_f.c \
        palltoallw_init_f.c \
        pbarrier_init_f.c \
        pbcast_init_f.c \
        pexscan_init_f.c \
        pgather_init_f.c \
        pgatherv_init_f.c \
        preduce_init_f.c \
        preduce_scatter_init_f.c \
        preduce_scatter_block_init_f.c \
        pscan_init_f.c \
        pscatter_init_f.c \
        pscatterv_init_f.c \
        \
	pneighbor_allgather_init_f.c \
        pneighbor_allgatherv_init_f.c \
        pneighbor_alltoall_init_f.c \
        pneighbor_alltoallv_init_f.c \
        pneighbor_alltoallw_init_f.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libpmpiext_pcollreq_mpifh_la_SOURCES):
	$(OMPI_V_LN_S) if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpiext/pcollreq/mpif-h/$$pname $@ ; \
	fi

endif
