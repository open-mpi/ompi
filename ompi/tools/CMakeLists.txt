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

SET(OMPI_WRAPPER_EXTRA_LIBS "advapi32.lib Ws2_32.lib shlwapi.lib")
GET_FILENAME_COMPONENT(CC "${CL_EXE}" NAME)
SET(OMPI_WRAPPER_EXTRA_CFLAGS 
  "/MDd /TC /D \"OMPI_IMPORTS\" /D \"OPAL_IMPORTS\" /D \"ORTE_IMPORTS\"")

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
  SET(OMPI_WRAPPER_EXTRA_CXXFLAGS 
    "/MDd /TP /EHsc /D \"OMPI_IMPORTS\" /D \"OPAL_IMPORTS\" /D \"ORTE_IMPORTS\"")
  SET(OMPI_WRAPPER_CXX_LIB "libmpi_cxx.lib")
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

