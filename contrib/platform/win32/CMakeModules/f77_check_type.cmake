# Copyright (c) 2008      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_F77_CHECK_TYPE
#         in: TYPE         - fortran type to check.
#        out: HAVE_TYPE    - 0/1 whether we have that type.
# -----------------------------------------------------------------

MACRO(OMPI_F77_CHECK_TYPE TYPE HAVE_TYPE)

  MESSAGE(STATUS "Check if Fortran 77 compiler supports ${TYPE}...")

  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_fortran_type.f
       "\t program main \n"
       "\t ${TYPE} bogus_variable \n"
       "\t END \n")

  EXECUTE_PROCESS(COMMAND ${F77} check_fortran_type.f
                  WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
                  OUTPUT_VARIABLE    OUTPUT
                  RESULT_VARIABLE    RESULT
                  ERROR_VARIABLE     ERROR)

  IF(RESULT)
    SET(${HAVE_TYPE} 0)
    MESSAGE(STATUS "Check if Fortran 77 compiler supports ${TYPE}...failed")
  ELSE(RESULT)
    SET(${HAVE_TYPE} 1)
    MESSAGE(STATUS "Check if Fortran 77 compiler supports ${TYPE}...done")
  ENDIF(RESULT)

ENDMACRO(OMPI_F77_CHECK_TYPE TYPE HAVE_TYPE)
