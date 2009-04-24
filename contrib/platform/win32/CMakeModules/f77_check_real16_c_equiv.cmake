# Copyright (c) 2008      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_F77_CHECK_REAL16_C_EQUIV
# ----------------------------------------------------
MACRO(OMPI_F77_CHECK_REAL16_C_EQUIV)
  SET(OMPI_REAL16_MATCHES_C 0)
  #MESSAGE(STATUS "OMPI_HAVE_FORTRAN_REAL16:${OMPI_HAVE_FORTRAN_REAL16}")

  IF(OMPI_WANT_F77_BINDINGS)
    IF(OMPI_HAVE_FORTRAN_REAL16)
      OMPI_F77_MAKE_C_FUNCTION(c ompi_ac_c_fn)
      FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_c.c
        "#include <stdio.h>"
        "#include <stdlib.h>"

        "#ifdef __cplusplus"
        "extern \"C\" {"
        "#endif"
        "void ${ompi_ac_c_fn}($OMPI_FORTRAN_REAL16_C_TYPE *a) {"
        "    FILE *fp = fopen(\"conftestval\", \"w\");"
        "    if (NULL == fp) exit(1);"
        "    fprintf(fp, \"%s\n\", (1.1L == *a) ? \"yes\" : \"no\");"
        "    fclose(fp);"
        "}"
        "#ifdef __cplusplus"
        "}"
        "#endif")
      
      FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest_f.f 
        "\t program bogus"
        "\t REAL*16 :: foo"
        "\t foo = 1.1"
        "\t call c(foo)"
        "\t end program bogus")

      EXECUTE_PROCESS(COMMAND ${CL_EXE} /c conftest_c.c /I${VC_INCLUDE_PATH}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
        OUTPUT_VARIABLE OUTPUT
        RESULT_VARIABLE RESULT
        ERROR_VARIABLE ERROR)
      
      EXECUTE_PROCESS(COMMAND ${F77} conftest_f.f conftest_c.obj -o conftest
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
        OUTPUT_VARIABLE OUTPUT
        RESULT_VARIABLE RESULT
        ERROR_VARIABLE ERROR)

      EXECUTE_PROCESS(COMMAND conftest.exe
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
        OUTPUT_VARIABLE OUTPUT
        RESULT_VARIABLE RESULT
        ERROR_VARIABLE ERROR)

      IF(RESULT)
        MESSAGE(FATAL_ERROR "Can not determine if REAL*16 bit-matches C.")
      ELSE(RESULT)
        IF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
          # read out type size value from the file, and write back to the output variable
          FILE(READ ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval ${OUTPUT_VARIABLE})
          MESSAGE(STATUS "Check if REAL*16 bit-matches C...${OUTPUT_VARIABLE}")
          SET(OMPI_REAL16_MATCHES_C 1)
        ELSE(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
          MESSAGE(STATUS "Check if REAL*16 bit-matches C...failed")
        ENDIF(EXISTS ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftestval)
      ENDIF(RESULT)
      
    ELSE(OMPI_HAVE_FORTRAN_REAL16)
      MESSAGE(STATUS "Check if REAL*16 bit-matches C...skipped")
    ENDIF(OMPI_HAVE_FORTRAN_REAL16)
    
  ENDIF(OMPI_WANT_F77_BINDINGS)
ENDMACRO(OMPI_F77_CHECK_REAL16_C_EQUIV)
