# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#PROJECT(CHELLO C CXX)

SET(CHELLO_FILES
  chello.c
)

ADD_EXECUTABLE (chello ${CHELLO_FILES})

TARGET_LINK_LIBRARIES (chello libmpi)

INSTALL(FILES ${OpenMPI_SOURCE_DIR}/test/mpi/environment/chello.c
  ${OpenMPI_BINARY_DIR}/test/mpi/environment/chello.*
  DESTINATION share/examples)