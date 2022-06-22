#
# Copyright (c) 2016-2022 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2021 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# If OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we definitely need it to be 1.
AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

noinst_LTLIBRARIES = libpmpiext_ftmpi_c.la

nodist_libpmpiext_ftmpi_c_la_SOURCES = \
    pcomm_revoke.c \
    pcomm_is_revoked.c \
    pcomm_shrink.c \
    pcomm_get_failed.c \
    pcomm_ack_failed.c \
    pcomm_failure_ack.c \
    pcomm_failure_get_acked.c \
    pcomm_agree.c \
    pcomm_iagree.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libpmpiext_ftmpi_c_la_SOURCES):
	$(OMPI_V_LN_S) if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpiext/ftmpi/c/$$pname $@ ; \
	fi


# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libpmpiext_ftmpi_c_la_SOURCES)
