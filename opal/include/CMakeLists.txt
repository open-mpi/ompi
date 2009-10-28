#
# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
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
  SET(OPAL_HAVE_LONG_LONG 1)
ENDIF(HAVE_LONG_LONG)



# make a temporary copy of the input file
FILE(READ ${OpenMPI_SOURCE_DIR}/ompi/include/mpi.h.in MPI_H_IN)

# change the syntax of the copy into CMake understandable
STRING(REGEX REPLACE "#undef ([-_a-zA-Z0-9]+)[ ]*\n" "#cmakedefine \\1 \${\\1}\n" MPI_H_OUT "${MPI_H_IN}")
STRING(REPLACE "{OMPI_MAJOR_VERSION}" "{OMPI_MAJOR_VERSION_STRING}" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "{OMPI_MINOR_VERSION}" "{OMPI_MINOR_VERSION_STRING}" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "{OMPI_RELEASE_VERSION}" "{OMPI_RELEASE_VERSION_STRING}" MPI_H_OUT "${MPI_H_OUT}")
STRING(REPLACE "#cmakedefine OMPI_HAVE_FORTRAN_" "#define OMPI_HAVE_FORTRAN_"  MPI_H_OUT "${MPI_H_OUT}")

FILE(WRITE ${CMAKE_FILES_DIRECTORY}/CMakeTmp/mpi.h.in "${MPI_H_OUT}")

# generate mpi.h
CONFIGURE_FILE(${CMAKE_FILES_DIRECTORY}/CMakeTmp/mpi.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpi.h)

IF(OMPI_WANT_F77_BINDINGS)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/mpif.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpif.h)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/mpif-config.h.in ${OpenMPI_BINARY_DIR}/ompi/include/mpif-config.h)
ENDIF(OMPI_WANT_F77_BINDINGS)
