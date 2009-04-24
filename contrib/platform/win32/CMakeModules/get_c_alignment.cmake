#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MACRO(C_GET_ALIGNMENT TYPE LANG NAME)

  IF(NOT ${NAME}_ALIGNMENT)

    #
    # Try to compile and run a foo grogram. 
    # The alignment result will be stored in ${CHECK_TYPE}_ALIGNMENT
    #

    MESSAGE( STATUS "Check alignment of ${TYPE} in ${LANG}...")
    
    FILE (WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/c_get_${NAME}_alignment.${LANG}"
      "#include <stddef.h>
       #include <stdio.h>
       #include <stdlib.h>
       int main(){
       char diff;
       struct foo {char a; ${TYPE} b;};
       struct foo *p = (struct foo *) malloc(sizeof(struct foo));
       diff = ((char *)&p->b) - ((char *)&p->a);
       return diff;}
    ")

    TRY_RUN(${NAME}_ALIGNMENT COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
      "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/c_get_${NAME}_alignment.${LANG}")

    MESSAGE( STATUS "Check alignment of ${TYPE} in ${LANG}...${${NAME}_ALIGNMENT}")

  ENDIF(NOT ${NAME}_ALIGNMENT)
ENDMACRO(C_GET_ALIGNMENT TYPE TYPE_ALIGNMENT LANG )
