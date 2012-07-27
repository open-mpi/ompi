# Copyright (c) 2007-2012 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

IF(OMPI_BUILD_CXX_BINDINGS)
  ADD_SUBDIRECTORY(cxx)
ELSE(OMPI_BUILD_CXX_BINDINGS)
  MESSAGE(STATUS "Skipping MPI C++ interface")
ENDIF(OMPI_BUILD_CXX_BINDINGS)

IF(OMPI_WANT_FORTRAN_BINDINGS)
  ADD_SUBDIRECTORY(fortran/mpif-h)
ELSE(OMPI_WANT_FORTRAN_BINDINGS)
  MESSAGE(STATUS "Skipping MPI FORTRAN interface")
ENDIF(OMPI_WANT_FORTRAN_BINDINGS)
