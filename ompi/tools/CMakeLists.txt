# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

ADD_SUBDIRECTORY(ompi_info)
ADD_SUBDIRECTORY(ompi-server)

# select debug or release libraries at configure time
IF(CMAKE_BUILD_TYPE STREQUAL "Debug")
  SET(OMPI_WRAPPER_EXTRA_LIBS
    "libmpid.lib libopen-pald.lib libopen-rted.lib advapi32.lib Ws2_32.lib shlwapi.lib")
ELSE(CMAKE_BUILD_TYPE STREQUAL "Debug")
  SET(OMPI_WRAPPER_EXTRA_LIBS
    "libmpi.lib libopen-pal.lib libopen-rte.lib advapi32.lib Ws2_32.lib shlwapi.lib")
ENDIF(CMAKE_BUILD_TYPE STREQUAL "Debug")

GET_FILENAME_COMPONENT(CC "${CL_EXE}" NAME)
IF(BUILD_SHARED_LIBS)
  SET(OMPI_WRAPPER_EXTRA_CFLAGS
    "/TC /D \"OMPI_IMPORTS\" /D \"OPAL_IMPORTS\" /D \"ORTE_IMPORTS\"")
ELSE(BUILD_SHARED_LIBS)
  IF(CMAKE_BUILD_TYPE STREQUAL "Debug")
    SET(OMPI_WRAPPER_EXTRA_CFLAGS "/TC /MDd")
  ELSE(CMAKE_BUILD_TYPE STREQUAL "Debug")
    SET(OMPI_WRAPPER_EXTRA_CFLAGS "/TC /MD")
  ENDIF(CMAKE_BUILD_TYPE STREQUAL "Debug")
ENDIF(BUILD_SHARED_LIBS)

CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpicc-wrapper-data.txt.cmake
               ${OMPI_BINARY_DIR}/tools/wrappers/mpicc-wrapper-data.txt @ONLY)
INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpicc-wrapper-data.txt
        DESTINATION share/openmpi)
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpicc-wrapper-data.txt.cmake
               ${OMPI_BINARY_DIR}/tools/wrappers/mpicc.exe-wrapper-data.txt @ONLY)
INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpicc.exe-wrapper-data.txt
        DESTINATION share/openmpi)

IF(OMPI_WANT_CXX_BINDINGS)
  GET_FILENAME_COMPONENT(CXX "${CL_EXE}" NAME)

  IF(BUILD_SHARED_LIBS)
    SET(OMPI_WRAPPER_EXTRA_CXXFLAGS 
      "/TP /EHsc /D \"OMPI_IMPORTS\" /D \"OPAL_IMPORTS\" /D \"ORTE_IMPORTS\"")
  ELSE(BUILD_SHARED_LIBS)
    SET(OMPI_WRAPPER_EXTRA_CXXFLAGS "/TP /EHsc")
  ENDIF(BUILD_SHARED_LIBS)

  # select debug or release libraries at configure time
  IF(CMAKE_BUILD_TYPE STREQUAL "Debug")
    SET(OMPI_WRAPPER_CXX_LIB "libmpid.lib libopen-pald.lib libopen-rted.lib libmpi_cxxd.lib")
  ELSE(CMAKE_BUILD_TYPE STREQUAL "Debug")
    SET(OMPI_WRAPPER_CXX_LIB "libmpi.lib libopen-pal.lib libopen-rte.lib libmpi_cxx.lib")
  ENDIF(CMAKE_BUILD_TYPE STREQUAL "Debug")

  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpic++-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpic++-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpic++-wrapper-data.txt
          DESTINATION share/openmpi)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpic++-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpicxx-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpicxx-wrapper-data.txt
          DESTINATION share/openmpi)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpic++-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpic++.exe-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpic++.exe-wrapper-data.txt
          DESTINATION share/openmpi)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpic++-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpicxx.exe-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpicxx.exe-wrapper-data.txt
          DESTINATION share/openmpi)
ENDIF(OMPI_WANT_CXX_BINDINGS)

IF(OMPI_WANT_F77_BINDINGS)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpif77-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpif77-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpif77-wrapper-data.txt
          DESTINATION share/openmpi)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpif77-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpif77.exe-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpif77.exe-wrapper-data.txt
          DESTINATION share/openmpi)
ENDIF(OMPI_WANT_F77_BINDINGS)

IF(OMPI_WANT_F90_BINDINGS)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpif90-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpif90-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpif90-wrapper-data.txt
          DESTINATION share/openmpi)
  CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/mpif90-wrapper-data.txt.cmake
                 ${OMPI_BINARY_DIR}/tools/wrappers/mpif90.exe-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/mpif90.exe-wrapper-data.txt
          DESTINATION share/openmpi)
ENDIF(OMPI_WANT_F90_BINDINGS)
