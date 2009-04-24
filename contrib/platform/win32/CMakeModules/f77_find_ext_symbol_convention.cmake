# Copyright (c) 2008      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MACRO(OMPI_F77_FIND_EXT_SYMBOL_CONVENTION)
  IF(OMPI_WANT_F77_BINDINGS)
    SET(OMPI_F77_DOUBLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - double underscore")
    SET(OMPI_F77_SINGLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - single underscore")
    SET(OMPI_F77_CAPS 0
      CACHE INTERNAL "external symbol convention - captital")
    SET(OMPI_F77_PLAIN 0
      CACHE INTERNAL "external symbol convention - plain")

    # first check if we have already detected dumpbin.exe
    IF(NOT DUMPBIN_EXE)
      MESSAGE(FATAL_ERROR "could not find dumpbin, cannot continue...")
    ENDIF(NOT DUMPBIN_EXE)

    # make sure we know our linking convention...
    MESSAGE(STATUS "Check ${CMAKE_Fortran_COMPILER} external symbol convention...")
    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest.f
      "\t subroutine FOO_bar(a) \n"
      "\t integer a \n"
      "\t a = 1 \n"
      "\t return \n"
      "\t end \n")
    
    EXECUTE_PROCESS(COMMAND ${F77} -c conftest.f -o conftest.lib
      WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      OUTPUT_VARIABLE    OUTPUT
      RESULT_VARIABLE    RESULT
      ERROR_VARIABLE     ERROR)

    SET(OUTPUT_OBJ_FILE "conftest.lib")

    # now run dumpbin to generate an output file
    EXECUTE_PROCESS(COMMAND ${DUMPBIN_EXE} ${OUTPUT_OBJ_FILE} /symbols /out:conftest_out
      WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      OUTPUT_VARIABLE    OUTPUT
      RESULT_VARIABLE    RESULT
      ERROR_VARIABLE     ERROR)

    # find out the external symbol convention
    FILE(STRINGS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_out 
      DOUBLE_UNDERSCORE REGEX "foo_bar__$")
    FILE(STRINGS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_out 
      SINGLE_UNDERSCORE REGEX "foo_bar_$")
    FILE(STRINGS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_out 
      MIXED_CASE REGEX "FOO_bar$")
    FILE(STRINGS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_out 
      NO_UNDERSCORE REGEX "foo_bar$")
    FILE(STRINGS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_out 
      UPPER_CASE REGEX "FOO_BAR$")

    # set up the corresponding values
    IF(NOT DOUBLE_UNDERSCORE STREQUAL "")
      SET(OMPI_F77_DOUBLE_UNDERSCORE 1
        CACHE INTERNAL "external symbol convention - double underscore")
      SET(FUNC_NAME "foo_bar__")
      SET(ompi_cv_f77_external_symbol "double underscore"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT SINGLE_UNDERSCORE STREQUAL "")
      SET(OMPI_F77_SINGLE_UNDERSCORE 1
        CACHE INTERNAL "external symbol convention - single underscore")
      SET(FUNC_NAME "foo_bar_")
      SET(ompi_cv_f77_external_symbol "single underscore"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT MIXED_CASE STREQUAL "")
      SET(OMPI_F77_CAPS 1
        CACHE INTERNAL "external symbol convention - captital")
      SET(FUNC_NAME "FOO_bar")
      SET(ompi_cv_f77_external_symbol "mixed case"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT NO_UNDERSCORE STREQUAL "")
      SET(OMPI_F77_PLAIN 1
        CACHE INTERNAL "external symbol convention - plain")
      SET(FUNC_NAME "foo_bar")
      SET(ompi_cv_f77_external_symbol "no underscore"
        CACHE INTERNAL "F77 external symbol convention")
    ELSEIF(NOT UPPER_CASE STREQUAL "")
      SET(OMPI_F77_CAPS 1
        CACHE INTERNAL "external symbol convention - captital")
      SET(FUNC_NAME "FOO_BAR")
      SET(ompi_cv_f77_external_symbol "upper case"
        CACHE INTERNAL "F77 external symbol convention")
    ELSE(NOT UPPER_CASE STREQUAL "")
      MESSAGE(FATAL_ERROR "unknow Fortran naming convertion.")
      SET(ompi_cv_f77_external_symbol "unknow")
    ENDIF(NOT DOUBLE_UNDERSCORE STREQUAL "")

    MESSAGE(STATUS "Check ${CMAKE_Fortran_COMPILER} external symbol convention...${ompi_cv_f77_external_symbol}")

    # now test if we can link the library with c program
    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_c.c 
      "int main(){${FUNC_NAME}();return(0);}")

    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/CMakeLists.txt
      "PROJECT(conftest_c C)\n" 
      "ADD_EXECUTABLE(conftest_c conftest_c.c)\n"
      "TARGET_LINK_LIBRARIES(conftest_c ${OUTPUT_OBJ_FILE})\n")

    TRY_COMPILE(
      TEST_OK
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
      conftest_c
      OUTPUT_VARIABLE MY_OUTPUT)

    #MESSAGE("MY_OUTPUT:${MY_OUTPUT}")

    IF(NOT TEST_OK)
      MESSAGE(FATAL_ERROR "C and Fortran 77 compilers are not link compatible.  Can not continue.")
    ENDIF(NOT TEST_OK)
  ELSE(OMPI_WANT_F77_BINDINGS)
    SET(OMPI_F77_DOUBLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - double underscore")
    SET(OMPI_F77_SINGLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - single underscore")
    SET(OMPI_F77_CAPS 0
      CACHE INTERNAL "external symbol convention - captital")
    SET(OMPI_F77_PLAIN 0
      CACHE INTERNAL "external symbol convention - plain")
  ENDIF(OMPI_WANT_F77_BINDINGS)

ENDMACRO(OMPI_F77_FIND_EXT_SYMBOL_CONVENTION)


# return the corresponding C function name
# OMPI_F77_MAKE_C_FUNCTION
#             in: FUNCTION_NAME     -Fortran function name
#            out: OUTPUT_VARIABLE   -C function name
MACRO(OMPI_F77_MAKE_C_FUNCTION OUTPUT_VARIABLE FUNCTION_NAME)
  IF("${ompi_cv_f77_external_symbol}" STREQUAL "double underscore")
    # so the general rule is that if there is an _ in the function
    # name, then there are two trailing underscores.  Otherwise,
    # there is only one trailing underscore.
    STRING(TOLOWER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
    STRING(REGEX MATCH "_" RESULT ${FUNCTION_NAME})
    IF("${RESULT}" STREQUAL "")
      SET(${OUTPUT_VARIABLE} "${${OUTPUT_VARIABLE}}_")
    ELSE("${RESULT}" STREQUAL "")
      SET(${OUTPUT_VARIABLE} "${${OUTPUT_VARIABLE}}__")
    ENDIF("${RESULT}" STREQUAL "")
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "single underscore")
    STRING(TOLOWER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
    SET(${OUTPUT_VARIABLE} "${OUTPUT_VARIABLE}_")
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "mixed case")
    SET(${OUTPUT_VARIABLE} ${FUNCTION_NAME})
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "no underscore")
    STRING(TOLOWER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
  ELSEIF("${ompi_cv_f77_external_symbol}" STREQUAL "upper case")
    STRING(TOUPPER ${FUNCTION_NAME} ${OUTPUT_VARIABLE})
  ELSE("${ompi_cv_f77_external_symbol}" STREQUAL "double underscore")
    MESSAGE(FATAL_ERROR "unknown naming convention: ${ompi_cv_f77_external_symbol}")
  ENDIF("${ompi_cv_f77_external_symbol}" STREQUAL "double underscore")

ENDMACRO(OMPI_F77_MAKE_C_FUNCTION OUTPUT_VARIABLE FUNCTION_NAME)
