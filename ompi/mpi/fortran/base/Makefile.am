# -*- makefile -*-
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

noinst_LTLIBRARIES =

EXTRA_DIST = \
     attr-fn-int-callback-interfaces.h \
     conversion-fn-null-int-interface.h \
     gen-mpi-sizeof.pl

#-----------------------------------------------------------------------------

# If we're building any Fortran bindings, then we need to build the
# Fortran glue that will get slurped into libmpi
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS
noinst_LTLIBRARIES += libmpi_fortran_base.la

# This needs to be its own convenience library because it requires
# some specialized CPPFLAGS to set all the defines in various .h files
# properly (copied from ompi/mpi/fortran/mpif-h/Makefile.am) -- we
# unfortunately can't just suck the sources into the larger libmpi.la.

libmpi_fortran_base_la_CPPFLAGS = \
        -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1
libmpi_fortran_base_la_SOURCES = \
        constants.h \
        datarep.h \
        fint_2_int.h \
        strings.h \
        attr_fn_f.c \
        conversion_fn_null_f.c \
        f90_accessors.c \
        strings.c \
        test_constants_f.c
endif
