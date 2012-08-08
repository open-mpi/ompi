# Copyright (c) 2007-2012 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


ADD_SUBDIRECTORY(chello)


FILE(READ ${OpenMPI_SOURCE_DIR}/contrib/platform/win32/examples/chello/chello.vcxproj EXAMPLE_SOLUTION)

IF(CMAKE_CL_64)
  STRING(REPLACE "Win32" "X64"  EXAMPLE_SOLUTION "${EXAMPLE_SOLUTION}")
  STRING(REPLACE "X86" "X64"  EXAMPLE_SOLUTION "${EXAMPLE_SOLUTION}")
ENDIF(CMAKE_CL_64)

FILE(WRITE ${OpenMPI_SOURCE_DIR}/contrib/platform/win32/examples/chello/chello.vcxproj "${EXAMPLE_SOLUTION}")

INSTALL(FILES ${OpenMPI_SOURCE_DIR}/contrib/platform/win32/examples/chello/chello.c
  ${OpenMPI_SOURCE_DIR}/contrib/platform/win32/examples/chello/chello.vcxproj
  DESTINATION share/examples)
