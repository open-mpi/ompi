# Copyright (c) 2008      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_F77_GET_SIZEOF(type, variable to set)
# ------------------------------------------

INCLUDE(F77_find_ext_symbol_convention)

MACRO(OMPI_F77_GET_SIZEOF TYPE OUTPUT_VARIABLE)
  MESSAGE(STATUS "Check size of Fortran 77 ${TYPE}...")
  
  OMPI_F77_MAKE_C_FUNCTION(ompi_ac_size_fn size)
  
  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.f
    "\t program fsize \n"
    "\t external size \n "
    "\t ${TYPE} x(2) \n"
    "\t call size(x(1),x(2)) \n"
    "\t end \n")

  IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.h)
    SET(ompi_conftest_h "#include \"conftest.h\"")
  ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.h)
    SET(ompi_conftest_h "")    
  ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.h)

  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.c
       "#include <stdio.h> \n"
       "#include <stdlib.h> \n"
       "${ompi_conftest_h} \n"
       "#ifdef __cplusplus \n"
       "extern \"C\" { \n"
       "#endif \n"
       ""
       "void ${ompi_ac_size_fn}(char *a, char *b) \n"
       "{ \n"
       "    int diff = (int) (b - a); \n"
       "    FILE *f=fopen(\"conftestval\", \"w\"); \n"
       "    if (!f) exit(1); \n"
       "    fprintf(f, \"%d\\n\", diff); \n"
       "    fclose(f); \n"
       "} \n"
       "#ifdef __cplusplus \n"
       "} \n"
       "#endif \n")

  EXECUTE_PROCESS(COMMAND ${CL_EXE} /c conftest.c /I${VC_INCLUDE_PATH}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
    OUTPUT_VARIABLE OUTPUT
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE ERROR)
  
  EXECUTE_PROCESS(COMMAND ${F77} conftest.f conftest.obj -o conftest
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
    OUTPUT_VARIABLE OUTPUT
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE ERROR)

  EXECUTE_PROCESS(COMMAND conftest.exe
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
    OUTPUT_VARIABLE OUTPUT
    RESULT_VARIABLE RESULT
    ERROR_VARIABLE ERROR)

#  MESSAGE("RESULT:${RESULT}")

  IF(RESULT)
    MESSAGE(FATAL_ERROR "Could not determine size of ${TYPE}.")
  ELSE(RESULT)
    IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
      # read out type size value from the file, and write back to the output variable
      FILE(READ ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval ${OUTPUT_VARIABLE})
      MESSAGE(STATUS "Check size of Fortran 77 ${TYPE}...${${OUTPUT_VARIABLE}}")
    ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
      MESSAGE(FATAL_ERROR "Could not determine size of ${TYPE}.")
    ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
  ENDIF(RESULT)

ENDMACRO(OMPI_F77_GET_SIZEOF TYPE OUTPUT_VARIABLE)
