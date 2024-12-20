#
# Copyright (c) 2021      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_BUILD_MPI_PROFILING is enabled when we want our generated MPI_* symbols
# to be replaced by PMPI_*.
# In this directory, we need it to be 0

AM_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

noinst_LTLIBRARIES = libpmpiext_continue_c.la

nodist_libpmpiext_continue_c_la_SOURCES = \
    pcontinue.c \
    pcontinueall.c \
    pcontinue_init.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libpmpiext_continue_c_la_SOURCES):
	$(OMPI_V_LN_S) if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpiext/continue/c/$$pname $@ ; \
	fi


# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libpmpiext_continue_c_la_SOURCES)
