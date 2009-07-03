# -*- cmake-script -*-
#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
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
    SET(VC_BIN_PATH ${VC_BIN_PATH}
      "C:/Program Files/Microsoft Visual Studio 9.0/VC/bin"
      "C:/Program Files (x86)/Microsoft Visual Studio 9.0/VC/bin"
      "C:/Program Files/Microsoft Visual Studio 8/VC/BIN"
      "C:/Program Files (x86)/Microsoft Visual Studio 8/VC/BIN"
      "C:/Program Files/Microsoft Visual Studio .NET 2003/VC7/BIN"
      "C:/Program Files (x86)/Microsoft Visual Studio .NET 2003/VC7/BIN"
      "$ENV{VS90COMNTOOLS}../../VC/bin"
      "$ENV{VS80COMNTOOLS}../../VC/bin"
      )
    
    # If we are using one of the Microsoft compilers check that we are
    # able to include windows.h. Most of the types that follow are defined
    # in this file. If we check for it here it will get included in the
    # default list of header files.
    FIND_PROGRAM(CL_EXE cl PATHS ${VC_BIN_PATH})

    # set the default include path for VC
    GET_FILENAME_COMPONENT(CL_EXE_PATH "${CL_EXE}" PATH)
    GET_FILENAME_COMPONENT(VC_INCLUDE_PATH "${CL_EXE_PATH}/../include" ABSOLUTE CACHE)
    GET_FILENAME_COMPONENT(VC_LIB_PATH "${CL_EXE_PATH}/../lib" ABSOLUTE CACHE)
    
    # the dumpbin path will be checked again when the f77 support is needed.
    FIND_PROGRAM(DUMPBIN_EXE dumpbin PATHS ${VC_BIN_PATH})
    
    # WHEN running dumpbin, it also needs the "Common7/IDE" directory in the
    # PATH. It will already be in the PATH if being run from a Visual Studio
    # command prompt. Add it to the PATH here in case we are running from a
    # different command prompt.
    GET_FILENAME_COMPONENT(DUMPBIN_EXE_DIR "${DUMPBIN_EXE}" PATH)
    GET_FILENAME_COMPONENT(DUMPBIN_EXE_DLLS_DIR "${DUMPBIN_EXE_DIR}/../../Common7/IDE" ABSOLUTE)
    
    IF(EXISTS "${DUMPBIN_EXE_DLLS_DIR}")
      # only add to the path if it is not already in the path
      IF(NOT "$ENV{PATH}" MATCHES "${DUMPBIN_EXE_DLLS_DIR}")
        SET(ENV{PATH} "$ENV{PATH};${DUMPBIN_EXE_DLLS_DIR}")
      ENDIF(NOT "$ENV{PATH}" MATCHES "${DUMPBIN_EXE_DLLS_DIR}")
    ENDIF(EXISTS "${DUMPBIN_EXE_DLLS_DIR}")

    FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/cl_test.c
      "int main() {return 0;}")
    
    TRY_COMPILE(CL_EXE_OK ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/cl_test.c)

    IF(CL_EXE_OK)

      MESSAGE( STATUS "Found Microsoft compiler: ${CL_EXE}.")

      CHECK_INCLUDE_FILE(windows.h HAVE_WINDOWS_H)
      CHECK_INCLUDE_FILE(winsock2.h HAVE_WINSOCK2_H)
      CHECK_INCLUDE_FILE(wdm.h HAVE_WDM_H)
      
      # The atomic functions are defined in a very unuasual manner.
      # Some of them are intrinsic defined in windows.h others are
      # exported by kernel32.dll. If we force the usage of TRY_RUN
      # here we will check for both in same time: compilation and run.

      # path of foo test programs
      SET (FOO_SOURCE_DIR ${OpenMPI_SOURCE_DIR}/CMakeTests)
      
      FOREACH(FUNCTION Exchange ExchangeAcquire ExchangeRelease Exchange64)
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
        
      ENDFOREACH(${FUNCTION})

      INCLUDE(check_c_type_exists)
      CHECK_C_TYPE_EXISTS(socklen_t SOCKLEN_T
        "winsock2.h;ws2tcpip.h") 

      CHECK_C_TYPE_EXISTS("struct sockaddr_in" STRUCT_SOCKADDR_IN
        "winsock2.h")
 
      CHECK_C_TYPE_EXISTS("struct sockaddr_in6" STRUCT_SOCKADDR_IN6
        "ws2tcpip.h")

      CHECK_C_TYPE_EXISTS("struct sockaddr_storage" STRUCT_SOCKADDR_STORAGE
        "winsock2.h;ws2tcpip.h")

    ELSE(CL_EXE_OK)
      MESSAGE(FATAL_ERROR "No working Microsoft compiler found. Please check if Visual Studio VC is correctly installed.")
    ENDIF(CL_EXE_OK)
    
    SET(MICROSOFT_CHECK_DONE TRUE CACHE INTERNAL "Microsoft check finished.")

  ENDIF(NOT MICROSOFT_CHECK_DONE)
  
ENDMACRO(OMPI_MICROSOFT_COMPILER)
