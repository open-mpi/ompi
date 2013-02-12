# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

IF(OMPI_WANT_CXX_BINDINGS)
#  ADD_SUBDIRECTORY(cxx)
  MESSAGE(STATUS "SHMEM C++ interface is not supported")
ELSE(OMPI_WANT_CXX_BINDINGS)
  MESSAGE(STATUS "Skipping SHMEM C++ interface")
ENDIF(OMPI_WANT_CXX_BINDINGS)

IF(OMPI_WANT_F77_BINDINGS)
#  ADD_SUBDIRECTORY(f77)
  MESSAGE(STATUS "Skipping SHMEM F77 interface is not supported")
ELSE(OMPI_WANT_F77_BINDINGS)
  MESSAGE(STATUS "Skipping SHMEM F77 interface")
ENDIF(OMPI_WANT_F77_BINDINGS)
