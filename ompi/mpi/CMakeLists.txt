# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

IF(OMPI_WANT_CXX_BINDINGS)
  ADD_SUBDIRECTORY(cxx)
ELSE(OMPI_WANT_CXX_BINDINGS)
  MESSAGE(STATUS "Skipping MPI C++ interface")
ENDIF(OMPI_WANT_CXX_BINDINGS)

IF(OMPI_WANT_FORTRAN_BINDINGS)
  ADD_SUBDIRECTORY(fortran)
ELSE(OMPI_WANT_FORTRAN_BINDINGS)
  MESSAGE(STATUS "Skipping MPI FORTRAN interface")
ENDIF(OMPI_WANT_FORTRAN_BINDINGS)
