# -*- cmake-script -*-
#
# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

######################################################################
#
# OMPI_MICROSOFT_COMPILER
#
# Keep all the Windows checks in one place.
#
# USAGE:
#   OMPI_MICROSOFT_COMPILER()
#
######################################################################
MACRO(OMPI_MICROSOFT_COMPILER)

  IF(NOT MICROSOFT_CHECK_DONE)

    MESSAGE( STATUS "Start Microsoft specific detection....")

    # search for Microsoft VC tools
    IF(CMAKE_CL_64)
      SET(CHECK_PATHS ${CHECK_PATHS}
        "C:/Program Files/Microsoft Visual Studio 9.0/VC/bin/amd64"
        "C:/Program Files (x86)/Microsoft Visual Studio 9.0/VC/bin/amd64"
        "C:/Program Files/Microsoft Visual Studio 8/VC/bin/amd64"
        "C:/Program Files (x86)/Microsoft Visual Studio 8/VC/bin/amd64"
        "C:/Program Files/Microsoft Visual Studio .NET 2003/VC7/bin/amd64"
        "C:/Program Files (x86)/Microsoft Visual Studio .NET 2003/VC7/bin/amd64"
        "$ENV{VS90COMNTOOLS}../../VC/bin/amd64"
        "$ENV{VS80COMNTOOLS}../../VC/bin//amd64"
        "$ENV{VS90COMNTOOLS}../../VC/bin/ia64"
        "$ENV{VS80COMNTOOLS}../../VC/bin//ia64")
    ELSE(CMAKE_CL_64)
      SET(CHECK_PATHS ${CHECK_PATHS}
        "C:/Program Files/Microsoft Visual Studio 9.0/VC/bin"
        "C:/Program Files (x86)/Microsoft Visual Studio 9.0/VC/bin"
        "C:/Program Files/Microsoft Visual Studio 8/VC/bin"
        "C:/Program Files (x86)/Microsoft Visual Studio 8/VC/bin"
        "C:/Program Files/Microsoft Visual Studio .NET 2003/VC7/bin"
        "C:/Program Files (x86)/Microsoft Visual Studio .NET 2003/VC7/bin"
        "$ENV{VS90COMNTOOLS}../../VC/bin"
        "$ENV{VS80COMNTOOLS}../../VC/bin")
    ENDIF(CMAKE_CL_64)

    FIND_PROGRAM(CL_EXE cl PATHS ${CHECK_PATHS})

    # Set up VS environments.
    GET_FILENAME_COMPONENT(VC_BIN_PATH ${CL_EXE} PATH)
    GET_FILENAME_COMPONENT(COMPILER_NAME ${CL_EXE} NAME)
    SET(CC ${COMPILER_NAME} CACHE INTERNAL "C compiler executable")
    SET(CXX ${COMPILER_NAME} CACHE INTERNAL "CXX compiler executable")
    GET_FILENAME_COMPONENT(SDK_ROOT_PATH
      "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Microsoft SDKs\\Windows;CurrentInstallFolder]" ABSOLUTE)
    IF(CMAKE_CL_64)
      SET(VS_ROOT_DIR ${VC_BIN_PATH}/../../..)
      SET(VC_LIB_DIR ${VS_ROOT_DIR}/VC/lib/amd64)
      SET(SDK_LIB_DIR ${SDK_ROOT_PATH}/lib/x64)
    ELSE(CMAKE_CL_64)
      SET(VS_ROOT_DIR ${VC_BIN_PATH}/../..)
      SET(VC_LIB_DIR ${VS_ROOT_DIR}/VC/lib)
      SET(SDK_LIB_DIR ${SDK_ROOT_PATH}/lib)
    ENDIF(CMAKE_CL_64)
    SET(VS_COMMON_TOOLS_DIR ${VS_ROOT_DIR}/Common7/Tools)
    SET(VS_IDE_DIR ${VS_ROOT_DIR}/Common7/IDE)
    SET(VC_INCLUDE_DIR ${VS_ROOT_DIR}/VC/include)


    # Cache the compilers paths that could be used later.
    SET(C_COMPILER_PATH ${VC_BIN_PATH} ${SDK_ROOT_PATH}/bin ${VS_IDE_DIR} CACHE INTERNAL "Compiler binary paths.")
    SET(C_COMPILER_INCLUDE ${VC_INCLUDE_DIR} CACHE INTERNAL "Compiler include paths.")
    SET(C_COMPILER_LIB ${VC_LIB_DIR} ${SDK_LIB_DIR} CACHE INTERNAL "Compiler libraries.")
    SET(C_COMPILER_LIBPATH ${VC_LIB_DIR} ${SDK_LIB_DIR} CACHE INTERNAL "Compiler libraries path.")

    SET(ENV{PATH} "${C_COMPILER_PATH};$ENV{PATH}")
    SET(ENV{INCLUDE} "${C_COMPILER_INCLUDE};$ENV{INCLUDE}")
    SET(ENV{LIB} "${C_COMPILER_LIB};$ENV{LIB}")
    SET(ENV{LIBPATH} "${C_COMPILER_LIBPATH};$ENV{LIBPATH}")

    # Default compiler settings.
    SET(OMPI_C_OPTION_COMPILE "/c" CACHE INTERNAL
      "C compiler option for compiling without linking.")
    SET(OMPI_C_OUTPUT_OBJ "/Fo" CACHE INTERNAL
      "C compiler option for setting object file name.")
    SET(OMPI_C_OUTPUT_EXE "/Fe" CACHE INTERNAL
      "C compiler option for setting executable file name.")

    SET(DUMP_UTIL "${VC_BIN_PATH}/dumpbin.exe" CACHE INTERNAL "the dumpbin application.")

    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/cl_test.c
      "int main() {return 0;}")

    TRY_COMPILE(CL_EXE_OK ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/cl_test.c)

    IF(CL_EXE_OK)

      # The atomic functions are defined in a very unuasual manner.
      # Some of them are intrinsic defined in windows.h others are
      # exported by kernel32.dll. If we force the usage of TRY_RUN
      # here we will check for both in same time: compilation and run.

      IF(${CMAKE_SYSTEM_VERSION} GREATER 5.1)
        SET(FUNCTION_LIST Exchange ExchangeAcquire ExchangeRelease Exchange64)
      ELSE(${CMAKE_SYSTEM_VERSION} GREATER 5.1)
        SET(FUNCTION_LIST Exchange ExchangeAcquire ExchangeRelease)
      ENDIF(${CMAKE_SYSTEM_VERSION} GREATER 5.1)

      FOREACH(FUNCTION ${FUNCTION_LIST})
        MESSAGE( STATUS "Checking for InterlockedCompare${FUNCTION}...")
        
        IF(FUNCTION STREQUAL "Exchange64")
          SET(64BITS_TYPE "LONGLONG" CACHE INTERNAL "64bits type longlong")
        ELSE(FUNCTION STREQUAL "Exchange64")
          SET(64BITS_TYPE "LONG" CACHE INTERNAL "64bits type long")
        ENDIF(FUNCTION STREQUAL "Exchange64")

        STRING(TOUPPER ${FUNCTION} FUNCTION_NAME)
        FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/InterlockedCompare${FUNCTION}_test.c
          "#include <windows.h>\n"
          "int main() {\n"
          "    ${64BITS_TYPE} dest = 0, exchange = 1, comperand = 0;\n"
          "    SetErrorMode(SEM_FAILCRITICALERRORS);\n"
          "    InterlockedCompare${FUNCTION}( &dest, exchange, comperand );\n"
          "    return (int)dest;\n"
          "    }\n")

        TRY_RUN (HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} COMPILE_RESULT 
          "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/" 
          "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/InterlockedCompare${FUNCTION}_test.c")

        IF(HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} STREQUAL "FAILED_TO_RUN" OR COMPILE_RESULT EQUAL FALSE)
          SET (HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} 0 CACHE INTERNAL "HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME}")
          MESSAGE( STATUS "Checking for InterlockedCompare${FUNCTION}...failed")
        ELSE(HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} STREQUAL "FAILED_TO_RUN" OR COMPILE_RESULT EQUAL FALSE)
          MESSAGE( STATUS "Checking for InterlockedCompare${FUNCTION}...done")
        ENDIF(HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} STREQUAL "FAILED_TO_RUN" OR COMPILE_RESULT EQUAL FALSE)

      ENDFOREACH(FUNCTION)

    ELSE(CL_EXE_OK)
      MESSAGE(FATAL_ERROR "No working Microsoft compiler found. Please check if Visual Studio VC is correctly installed.")
    ENDIF(CL_EXE_OK)

    SET(MICROSOFT_CHECK_DONE TRUE CACHE INTERNAL "Microsoft check finished.")

  ENDIF(NOT MICROSOFT_CHECK_DONE)

  OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGE "Whether we support 32 bits atomic operations on Windows" 0 0)
  OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGE64 "Whether we support 64 bits atomic operations on Windows" 0 0)
  OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE "Whether we support 32 bits atomic operations on Windows" 0 0)
  OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE "Whether we support 32 bits atomic operations on Windows" 0 0)

ENDMACRO(OMPI_MICROSOFT_COMPILER)
