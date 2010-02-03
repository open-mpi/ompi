# Copyright (c) 2008-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# first try to find a f77 compiler, will be checked when f77 support is enabled.

# There might be a bug in CMake, the CMAKE_GENERATOR_FC is set to "ifort" by default,
# which causes CMake can't find the correct Fortran compiler.
# We have to set CMAKE_GENERATOR_FC empty.
SET(CMAKE_GENERATOR_FC "")
include(CMakeDetermineFortranCompiler)
include(CMakeFortranInformation)

GET_FILENAME_COMPONENT(F77 ${CMAKE_Fortran_COMPILER} NAME_WE)
GET_FILENAME_COMPONENT(F77_PATH ${CMAKE_Fortran_COMPILER} PATH)

# Default compiler settings.
SET(OMPI_F77_OPTION_COMPILE "-c" CACHE STRING
  "Fortran compiler option for compiling without linking.")
SET(OMPI_F77_OUTPUT_OBJ "-o" CACHE STRING
  "Fortran compiler option for setting object file name.")
SET(OMPI_F77_OUTPUT_EXE "-o" CACHE STRING
  "Fortran compiler option for setting executable file name.")
SET(OMPI_F77_LIB_PATH "" CACHE PATH
  "Library path for the fortran compiler")
SET(OMPI_F77_INCLUDE_PATH "" CACHE PATH
  "Include path for the fortran compiler")

INCLUDE(F77_find_ext_symbol_convention)
# make sure we know the linking convention
# this macro will also test linking with C code
OMPI_F77_FIND_EXT_SYMBOL_CONVENTION()

IF(OMPI_WANT_F77_BINDINGS AND NOT F77_SETUP_DONE)

  # Export env variables for fortran compiler.
  SET(ENV{PATH} "${F77_PATH};$ENV{PATH}")
  SET(ENV{LIB} "${OMPI_F77_LIB_PATH};$ENV{LIB}")
  SET(ENV{INCLUDE} "${OMPI_F77_INCLUDE_PATH};$ENV{INCLUDE}")

  # make sure the compiler actually works, if not cross-compiling
  MESSAGE(STATUS "Checking for working Fortran compiler...")
  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f
       "\t PROGRAM TESTFortran \n"
       "\t PRINT *, 'Hello' \n"
       "\t END \n")

  # lets use execute_process to run the compile test
  EXECUTE_PROCESS(COMMAND ${CMAKE_Fortran_COMPILER} testFortranCompiler.f
                  WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
                  OUTPUT_VARIABLE    OUTPUT
                  RESULT_VARIABLE    RESULT
                  ERROR_VARIABLE     ERROR)

  IF(RESULT)
    MESSAGE("Fortran compiler ${F77} can't compile a simple fortran program.")
    MESSAGE(FATAL_ERROR "Cannot continue. Please check Fortran compiler installation, or disable Fortran 77 support.")
  ELSE(RESULT)
    MESSAGE(STATUS "Checking for working Fortran compiler...${F77}")
  ENDIF(RESULT)


  SET(F77_SETUP_DONE TRUE CACHE INTERNAL "f77 setup done.")

ENDIF(OMPI_WANT_F77_BINDINGS AND NOT F77_SETUP_DONE)
