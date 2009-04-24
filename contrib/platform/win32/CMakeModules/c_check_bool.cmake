#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
  
IF(NOT DEFINED COMPILER_SUPPORT_BOOL)
  MESSAGE( STATUS "Check whether the compiler supports bool...")

  FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_bool.c"
    "${INCLUDE_HEADERS}
     int main(){ ${TYPE} test; return sizeof(${TYPE});}")

  TRY_COMPILE(COMPILER_SUPPORT_BOOL "${CMAKE_BINARY_DIR}"
    "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_bool.c")

  IF(COMPILER_SUPPORT_BOOL)
    MESSAGE ( STATUS "Our compiler supports \"bool\".")
  ELSE(COMPILER_SUPPORT_BOOL)
    MESSAGE ( STATUS "Our compiler doesn't support \"bool\".")
  ENDIF(COMPILER_SUPPORT_BOOL)
ENDIF(NOT DEFINED COMPILER_SUPPORT_BOOL)
