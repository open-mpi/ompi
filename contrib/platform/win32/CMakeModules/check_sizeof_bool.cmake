#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MACRO(CHECK_SIZEOF_BOOL)

  IF(NOT DEFINED SIZEOF_BOOL)

    #
    # Try to compile and run a foo grogram, store the result in SIZEOF_BOOL.
    #

    MESSAGE( STATUS "Checking size of bool...")
    FOREACH(LANG c cxx)
      FILE (WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_sizeof_bool.${LANG}"
        "#include <stdio.h>
       int main() {return sizeof(bool);}
      ")
      
      TRY_RUN(SIZEOF_BOOL COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
        "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_sizeof_bool.${LANG}")
      
      IF(SIZEOF_BOOL GREATER 0)
        MESSAGE( STATUS "Checking size of bool...${SIZEOF_BOOL}")
        BREAK()
      ENDIF(SIZEOF_BOOL GREATER 0)
      
    ENDFOREACH(LANG C CXX)

    IF(SIZEOF_BOOL EQUAL 0)
      MESSAGE( STATUS "Checking size of bool...failed")
    ENDIF(SIZEOF_BOOL EQUAL 0)
    
  ENDIF(NOT DEFINED SIZEOF_BOOL)
ENDMACRO(CHECK_SIZEOF_BOOL)
