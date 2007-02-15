# -*- makefile -*-
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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# *_DATA files are not automatically included in the tarball.  Need to
# ensure that README.txt is picked up so that people who disable the
# C++ MPI bindings can still build properly.
EXTRA_DIST = README.txt

if WANT_MPI_CXX_BINDINGS

mpi_lib = libmpi_cxx.la

lib_LTLIBRARIES = libmpi_cxx.la 
libmpi_cxx_la_SOURCES = \
	mpicxx.cc \
	intercepts.cc \
	comm.cc \
	datatype.cc \
	file.cc \
	win.cc

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

ompidir = $(includedir)/openmpi/ompi/mpi/cxx
ompi_HEADERS = \
        $(headers)
else
# If the C++ bindings were disabled, still ensure that this directory
# is created so that the default -I flags in the wrapper compilers
# point to a directory that exists.
ompidir = $(includedir)/openmpi
ompi_DATA = README.txt
endif
