#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# Try to compile and run a test program.
# If the type is defined, then return the size of the type.
# 
# TYPE:             the type to check
# TYPE_NAME:        the uppercase of the type, with underlines if necessary.
# INCLUDE_HEADERS:  the header files defines the type.
# 
# HAVE_${TYPE_NAME}:    if type is found, this value is define as TRUE.
# SIZEOF_${TYPE_NAME}:  size of the type.

MACRO(CHECK_C_TYPE_EXISTS TYPE TYPE_NAME INCLUDE_HEADERS)

  MESSAGE( STATUS "Checking for ${TYPE}...")

  PROJECT(FOO)
  FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_${TYPE_NAME}.c"
    "${INCLUDE_HEADERS}
     int main(){ ${TYPE} test; return sizeof(${TYPE});}")

  TRY_RUN(SIZEOF_${TYPE_NAME} COMPILE_RESULT "${CMAKE_BINARY_DIR}"
    "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_${TYPE_NAME}.c")

  IF(SIZEOF_${TYPE_NAME} GREATER 0)
    SET(HAVE_${TYPE_NAME} TRUE CACHE INTERNAL "HAVE_${TYPE_NAME}")
    MESSAGE( STATUS "Checking for ${TYPE}...done")
  ELSE(SIZEOF_${TYPE_NAME} GREATER 0)
    SET(HAVE_${TYPE_NAME} FALSE CACHE INTERNAL "HAVE_${TYPE_NAME}")
    MESSAGE( STATUS "Checking for ${TYPE}...failed")
  ENDIF(SIZEOF_${TYPE_NAME} GREATER 0)

ENDMACRO(CHECK_C_TYPE_EXISTS TYPE TYPE_NAME INCLUDE_HEADERS)