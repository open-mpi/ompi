# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
#
# LAM_PROFILING_DEFINES flag is enabled when we want our MPI_* symbols
# to be replaced by PMPI_*. In other words, this flag decides 
# whether "profile/defines.h" is included or not. "profile/defines.h"
# replaces all MPI_* symbols with PMPI_* symbols. In this directory
# we definately need it to be 1.
#
AM_CPPFLAGS = -DLAM_PROFILE_LAYER=1
#
# This build needs to go through only if profiling is required.
# Further, this build HAS to go through if profiling is required.
#

if WANT_PMPI_BINDINGS_LAYER
pmpi_lib = libmpi_f77_pmpi.la
else
pmpi_lib = 
endif

noinst_LTLIBRARIES = $(pmpi_lib)

nodist_libmpi_f77_pmpi_la_SOURCES = \
        palloc_mem_f.c \
        pcomm_get_name_f.c \
        pcomm_set_name_f.c \
        pinit_f.c \
        pfinalize_f.c \
        pfree_mem_f.c
#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_f77_pmpi_la_SOURCES):
    if test ! -r $@ ; then \
        pname=`echo $@ | cut -b '2-'` ; \
        ln -s $(top_srcdir)/src/mpi/interface/f77/$$pname $@ ; \
    fi
#
# 
headers = \
    prototypes_pmpi.h
#
# These files were created by targets above
#
MAINTAINERCLEANFILES = $(nodist_libmpi_f77_profile_la_SOURCES)
#
# Conditionally install the header files
#
EXTRA_DIST = $(nodist_libmpi_f77_pmpi_la_SOURCES)
#
if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/f77/profile
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
#
# Don't want these targets in here
#
tags-recursive:
tags:
TAGS:
GTAGS:
ID:
