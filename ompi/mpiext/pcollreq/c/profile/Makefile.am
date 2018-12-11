#
# Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2018      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we need it to be 1

AM_CPPFLAGS     = -DOMPI_BUILD_MPI_PROFILING=1

include $(top_srcdir)/Makefile.ompi-rules

# Convenience libtool library that will be slurped up into libmpi.la.
noinst_LTLIBRARIES = libpmpiext_pcollreq_c.la

# This is where the top-level header file (that is included in
# <mpi-ext.h>) must be installed.
ompidir = $(ompiincludedir)/mpiext

# This is the header file that is installed.
ompi_HEADERS = pmpiext_pcollreq_c.h

# Sources for the convenience libtool library.  Other than the one
# header file, all source files in the extension have no file naming
# conventions.
nodist_libpmpiext_pcollreq_c_la_SOURCES = \
        $(ompi_HEADERS) \
        pallgather_init.c \
        pallgatherv_init.c \
        pallreduce_init.c \
        palltoall_init.c \
        palltoallv_init.c \
        palltoallw_init.c \
        pbarrier_init.c \
        pbcast_init.c \
        pexscan_init.c \
        pgather_init.c \
        pgatherv_init.c \
        preduce_init.c \
        preduce_scatter_block_init.c \
        preduce_scatter_init.c \
        pscan_init.c \
        pscatter_init.c \
        pscatterv_init.c \
        \
        pneighbor_allgather_init.c \
        pneighbor_allgatherv_init.c \
        pneighbor_alltoall_init.c \
        pneighbor_alltoallv_init.c \
        pneighbor_alltoallw_init.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libpmpiext_pcollreq_c_la_SOURCES):
	$(OMPI_V_LN_S) if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpiext/pcollreq/c/$$pname $@ ; \
	fi
