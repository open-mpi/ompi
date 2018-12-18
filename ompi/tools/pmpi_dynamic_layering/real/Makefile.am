#
# Copyright (c) 2016-2020 IBM Corporation. All rights reserved.
# $COPYRIGHT$
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

psupportdir = $(libdir)/profilesupport
psupport_LTLIBRARIES = libmpiprofilesupport.la
here_src = $(top_srcdir)/ompi/tools/pmpi_dynamic_layering/real

libmpiprofilesupport_la_SOURCES = wrap.c constructed_wrapper_done.h
#libmpiprofilesupport_la_LDFLAGS= -avoid-version
libmpiprofilesupport_la_LDFLAGS = -version-info $(libmpi_so_version)

nodist_prog_SOURCES = constructed_wrapper_done.h
BUILT_SOURCES = constructed_wrapper_done.h

OMPI_OMIT_MPI1_COMPAT_DECLS=1
if OMPI_ENABLE_MPI1_COMPAT
OMPI_OMIT_MPI1_COMPAT_DECLS=0
endif

# The mkcode.py script relies on finding some json output based off the MPI
# standard
constructed_wrapper_done.h: mkcode.py mpi_calls.json
	$(here_src)/mkcode.py

clean-local:
	rm -f constructed_wrapper_*.h
