# -*- makefile -*-
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

include $(top_srcdir)/config/Makefile.options

if WANT_MPI_CXX_BINDINGS

mpi_lib = libmpi_cxx.la

lib_LTLIBRARIES = libmpi_cxx.la 
libmpi_cxx_la_SOURCES = \
	mpicxx.cc \
	intercepts.cc \
	comm.cc

headers = \
	mpicxx.h \
        constants.h \
	file.h \
        functions.h \
        datatype.h \
        exception.h \
        op.h \
        status.h \
        request.h \
        group.h \
        comm.h \
        errhandler.h \
        intracomm.h \
        info.h \
        win.h \
        topology.h \
        intercomm.h \
        datatype_inln.h \
	file_inln.h \
        functions_inln.h \
        request_inln.h \
        comm_inln.h \
        intracomm_inln.h \
        info_inln.h \
        win_inln.h \
        topology_inln.h \
        intercomm_inln.h \
        group_inln.h \
        op_inln.h \
        errhandler_inln.h \
        status_inln.h

ompidir = $(includedir)/openmpi/mpi/cxx
ompi_HEADERS = \
	$(headers)
else
ompidir = $(includedir)
endif
