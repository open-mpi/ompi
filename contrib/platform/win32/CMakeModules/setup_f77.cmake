# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# first try to find a fortran compiler, will be checked when fortran support is enabled.

# There might be a bug in CMake, the CMAKE_GENERATOR_FC is set to "ifort" by default,
# which causes CMake can't find the correct Fortran compiler.
# We have to set CMAKE_GENERATOR_FC empty.
SET(CMAKE_GENERATOR_FC "")
include(CMakeDetermineFortranCompiler)
include(CMakeFortranInformation)

IF(OMPI_WANT_FORTRAN_BINDINGS AND NOT FORTRAN_SETUP_DONE)

  SET(OMPI_MPI_INTEGER_KIND 0 CACHE INTERNAL "MPI_INTEGER_KIND")
  SET(OMPI_MPI_ADDRESS_KIND 0 CACHE INTERNAL "MPI_ADDRESS_KIND")
  SET(OMPI_MPI_OFFSET_KIND 0 CACHE INTERNAL "MPI_OFFSET_KIND")
  SET(OMPI_FORTRAN_STATUS_SIZE 0 CACHE INTERNAL "MPI_STATUS_SIZE")

  GET_FILENAME_COMPONENT(FORTRAN_NAME ${CMAKE_Fortran_COMPILER} NAME)
  GET_FILENAME_COMPONENT(FORTRAN_PATH ${CMAKE_Fortran_COMPILER} PATH)

  SET(FORTRAN ${FORTRAN_NAME} CACHE INTERNAL "Name of the fortran compiler.")

  # Default compiler settings.
  IF(${FORTRAN} STREQUAL "ifort.exe")
    #settings for Intel Fortran
    SET(FORTRAN_OPTION_COMPILE "/c" CACHE INTERNAL
      "Fortran compiler option for compiling without linking.")
    SET(FORTRAN_OUTPUT_OBJ "/Fo" CACHE INTERNAL
      "Fortran compiler option for setting object file name.")
    SET(FORTRAN_OUTPUT_EXE "/Fe" CACHE INTERNAL
      "Fortran compiler option for setting executable file name.")
    SET(FORTRAN_DYNAMIC_FLAG_DEBUG "/MDd" CACHE INTERNAL
      "Compile flag for using dynamically-loaded, multithread C runtime (Debug).")
    SET(FORTRAN_DYNAMIC_FLAG "/MD" CACHE INTERNAL
      "Compile flag for using dynamically-loaded, multithread C runtime.")

    IF(NOT "$ENV{IFORT_COMPILER11}" STREQUAL "")
      SET(IFORT_LIB_PATH "$ENV{IFORT_COMPILER11}/lib/")
    ELSEIF(NOT "$ENV{IFORT_COMPILER12}" STREQUAL "")
      SET(IFORT_LIB_PATH "$ENV{IFORT_COMPILER12}/compiler/lib/")
    ENDIF(NOT "$ENV{IFORT_COMPILER11}" STREQUAL "")

    IF(CMAKE_CL_64)
      SET(FORTRAN_LIB_PATH "${IFORT_LIB_PATH}/intel64")
    ELSE(CMAKE_CL_64)
      SET(FORTRAN_LIB_PATH "${IFORT_LIB_PATH}/ia32")
    ENDIF(CMAKE_CL_64)

    IF(NOT FORTRAN_LIB_PATH)
      IF(CMAKE_CL_64)
        FIND_LIBRARY(FORTRAN_IFCONSOL_LIB ifconsol.lib PATHS ${FORTRAN_PATH}/../../intel64)
      ELSE(CMAKE_CL_64)
        FIND_LIBRARY(FORTRAN_IFCONSOL_LIB ifconsol.lib PATHS ${FORTRAN_PATH}/../../ia32)
      ENDIF(CMAKE_CL_64)
      GET_FILENAME_COMPONENT(FORTRAN_LIB_PATH ${FORTRAN_IFCONSOL_LIB} PATH)
      UNSET(FORTRAN_IFCONSOL_LIB CACHE)
    ELSE(NOT FORTRAN_LIB_PATH)
      STRING(REPLACE "\\" "/" FORTRAN_LIB_PATH ${FORTRAN_LIB_PATH})
    ENDIF(NOT FORTRAN_LIB_PATH)
  ELSEIF(${FORTRAN} STREQUAL "g95.exe")
    #settings for G95
    SET(FORTRAN_OPTION_COMPILE "-c" CACHE INTERNAL
      "Fortran compiler option for compiling without linking.")
    SET(FORTRAN_OUTPUT_OBJ "-o" CACHE INTERNAL
      "Fortran compiler option for setting object file name.")
    SET(FORTRAN_OUTPUT_EXE "-o" CACHE INTERNAL
      "Fortran compiler option for setting executable file name.")
  ELSE(${FORTRAN} STREQUAL "ifort.exe")
    # in other case, let user specify their fortran configrations.
    SET(FORTRAN_OPTION_COMPILE "-c" CACHE STRING
      "Fortran compiler option for compiling without linking.")
    SET(FORTRAN_OUTPUT_OBJ "-o" CACHE STRING
      "Fortran compiler option for setting object file name.")
    SET(FORTRAN_OUTPUT_EXE "-o" CACHE STRING
      "Fortran compiler option for setting executable file name.")
    SET(FORTRAN_LIB_PATH "" CACHE PATH
      "Library path for the fortran compiler")
    SET(FORTRAN_INCLUDE_PATH "" CACHE PATH
      "Include path for the fortran compiler")
  ENDIF(${FORTRAN} STREQUAL "ifort.exe")

  # Export env variables for fortran compiler.
  SET(ENV{PATH} "${C_COMPILER_PATH};${FORTRAN_PATH};$ENV{PATH}")
  SET(ENV{LIB} "${C_COMPILER_LIB};${FORTRAN_LIB_PATH};$ENV{LIB}")
  SET(ENV{INCLUDE} "${C_COMPILER_INCLUDE};${FORTRAN_INCLUDE_PATH};$ENV{INCLUDE}")
  SET(ENV{LIBPATH} "${C_COMPILER_LIBPATH};$ENV{LIBPATH}")

  # make sure the compiler actually works, if not cross-compiling
  MESSAGE(STATUS "Checking for working Fortran compiler...")
  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f
       "\t PROGRAM TESTFortran \n"
       "\t PRINT *, 'Hello' \n"
       "\t END \n")

  # lets use execute_process to run the compile test
  EXECUTE_PROCESS(COMMAND ${FORTRAN} testFortranCompiler.f
                  WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
                  OUTPUT_VARIABLE    OUTPUT
                  RESULT_VARIABLE    RESULT
                  ERROR_VARIABLE     ERROR)


  IF(RESULT)
    SET(FORTRAN_SETUP_DONE FALSE CACHE INTERNAL "fortran setup done.")
    MESSAGE(STATUS "${OUTPUT}\n${ERROR}")
    MESSAGE(STATUS "Fortran compiler ${FORTRAN} can't compile a simple fortran program.")
    MESSAGE(FATAL_ERROR "Cannot continue. Please check Fortran compiler installation, or disable Fortran 77 support.")
  ELSE(RESULT)
    MESSAGE(STATUS "Checking for working Fortran compiler...${FORTRAN}")
    SET(FORTRAN_SETUP_DONE TRUE CACHE INTERNAL "fortran setup done.")
  ENDIF(RESULT)

  INCLUDE(FORTRAN_find_ext_symbol_convention)
  # make sure we know the linking convention
  # this macro will also test linking with C code
  OMPI_FORTRAN_FIND_EXT_SYMBOL_CONVENTION()

ELSEIF(NOT OMPI_WANT_FORTRAN_BINDINGS)
    SET(OMPI_FORTRAN_DOUBLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - double underscore")
    SET(OMPI_FORTRAN_SINGLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - single underscore")
    SET(OMPI_FORTRAN_CAPS 0
      CACHE INTERNAL "external symbol convention - captital")
    SET(OMPI_FORTRAN_PLAIN 0
      CACHE INTERNAL "external symbol convention - plain")
    
    UNSET(SYMBOL_CONVENTION_CHECK_DONE CACHE)
    UNSET(FORTRAN_OPTION_COMPILE CACHE)
    UNSET(FORTRAN_OUTPUT_OBJ CACHE)
    UNSET(FORTRAN_OUTPUT_EXE CACHE)
    UNSET(FORTRAN_LIB_PATH CACHE)
    UNSET(FORTRAN_INCLUDE_PATH CACHE)
    UNSET(FORTRAN_IFCONSOL_LIB CACHE)
    UNSET(FORTRAN_SETUP_DONE CACHE)
ENDIF(OMPI_WANT_FORTRAN_BINDINGS AND NOT FORTRAN_SETUP_DONE)

# a few definitions needed by OMPI_FORTRAN_FIND_EXT_SYMBOL_CONVENTION check.
OMPI_DEF_VAR(OMPI_FORTRAN_DOUBLE_UNDERSCORE "Whether fortran symbols have a trailing double underscore or not." 0 1)
OMPI_DEF_VAR(OMPI_FORTRAN_SINGLE_UNDERSCORE "Whether fortran symbols have a trailing single underscore or not." 0 1)
OMPI_DEF_VAR(OMPI_FORTRAN_CAPS "Whether fortran symbols are all caps or not." 0 1)
OMPI_DEF_VAR(OMPI_FORTRAN_PLAIN "Whether fortran symbols have no trailing underscore or not." 0 1)

