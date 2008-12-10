#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/opal_config.h.cmake  ${OpenMPI_BINARY_DIR}/opal/include/opal_config.h)

IF(HAVE_LONG_LONG)
  SET(OMPI_HAVE_LONG_LONG 1)
ENDIF(HAVE_LONG_LONG)

# generate mpi.h and mpif.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpi.h.cmake ${OpenMPI_BINARY_DIR}/ompi/include/mpi.h)

IF(OMPI_WANT_F77_BINDINGS)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/mpif.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpif.h)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/mpif-config.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpif-config.h)
ENDIF(OMPI_WANT_F77_BINDINGS)
