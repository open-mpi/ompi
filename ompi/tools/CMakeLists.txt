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


MACRO(CONFIGURE_WRAPPER_DATA_FILE APP_NAME)

  # mpic++ and mpicxx share the same input file
  IF(${APP_NAME} STREQUAL "mpicxx")
    SET(INPUT_PREFIX "mpic++")
  ELSE(${APP_NAME} STREQUAL "mpicxx")
    SET(INPUT_PREFIX ${APP_NAME})
  ENDIF(${APP_NAME} STREQUAL "mpicxx")
  
  FILE(READ ${OpenMPI_SOURCE_DIR}/ompi/tools/wrappers/${INPUT_PREFIX}-wrapper-data.txt.in FILE_STRING)
  STRING(REGEX REPLACE "-l[-a-zA-Z0-9_]+ " "" OUTPUT ${FILE_STRING})

  # create a temporary input file, for "write file" command only
  # replace the variables in the first level. :(
  FILE(WRITE ${CMAKE_FILES_DIRECTORY}/CMakeTmp/${APP_NAME}-wrapper-data.txt.in "${OUTPUT}")

  CONFIGURE_FILE(${CMAKE_FILES_DIRECTORY}/CMakeTmp/${APP_NAME}-wrapper-data.txt.in
                 ${OMPI_BINARY_DIR}/tools/wrappers/${APP_NAME}-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/${APP_NAME}-wrapper-data.txt
          DESTINATION share/openmpi)
  CONFIGURE_FILE(${CMAKE_FILES_DIRECTORY}/CMakeTmp/${APP_NAME}-wrapper-data.txt.in
                 ${OMPI_BINARY_DIR}/tools/wrappers/${APP_NAME}.exe-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OMPI_BINARY_DIR}/tools/wrappers/${APP_NAME}.exe-wrapper-data.txt
          DESTINATION share/openmpi)
ENDMACRO()

# select debug or release libraries at configure time
IF(OMPI_DEBUG_BUILD)
  SET(OMPI_WRAPPER_EXTRA_LIBS
    "libmpid.lib libopen-pald.lib libopen-rted.lib advapi32.lib Ws2_32.lib shlwapi.lib")
ELSE(OMPI_DEBUG_BUILD)
  SET(OMPI_WRAPPER_EXTRA_LIBS
    "libmpi.lib libopen-pal.lib libopen-rte.lib advapi32.lib Ws2_32.lib shlwapi.lib")
ENDIF(OMPI_DEBUG_BUILD)

GET_FILENAME_COMPONENT(CC "${CL_EXE}" NAME)
IF(BUILD_SHARED_LIBS)
  SET(OMPI_WRAPPER_EXTRA_CFLAGS
    "/TC /D \"OMPI_IMPORTS\" /D \"OPAL_IMPORTS\" /D \"ORTE_IMPORTS\"")
ELSE(BUILD_SHARED_LIBS)
  IF(OMPI_DEBUG_BUILD)
    SET(OMPI_WRAPPER_EXTRA_CFLAGS "/TC /MDd")
  ELSE(OMPI_DEBUG_BUILD)
    SET(OMPI_WRAPPER_EXTRA_CFLAGS "/TC /MD")
  ENDIF(OMPI_DEBUG_BUILD)
ENDIF(BUILD_SHARED_LIBS)

CONFIGURE_WRAPPER_DATA_FILE(mpicc)

IF(OMPI_WANT_CXX_BINDINGS)
  GET_FILENAME_COMPONENT(CXX "${CL_EXE}" NAME)

  IF(BUILD_SHARED_LIBS)
    SET(OMPI_WRAPPER_EXTRA_CXXFLAGS 
      "/TP /EHsc /D \"OMPI_IMPORTS\" /D \"OPAL_IMPORTS\" /D \"ORTE_IMPORTS\"")
  ELSE(BUILD_SHARED_LIBS)
    SET(OMPI_WRAPPER_EXTRA_CXXFLAGS "/TP /EHsc")
  ENDIF(BUILD_SHARED_LIBS)

  # select debug or release libraries at configure time
  IF(OMPI_DEBUG_BUILD)
    SET(OMPI_WRAPPER_CXX_LIB "libmpi_cxxd.lib")
  ELSE(OMPI_DEBUG_BUILD)
    SET(OMPI_WRAPPER_CXX_LIB "libmpi_cxx.lib")
  ENDIF(OMPI_DEBUG_BUILD)

  CONFIGURE_WRAPPER_DATA_FILE(mpic++)
  CONFIGURE_WRAPPER_DATA_FILE(mpicxx)
ENDIF(OMPI_WANT_CXX_BINDINGS)

IF(OMPI_WANT_F77_BINDINGS)
  CONFIGURE_WRAPPER_DATA_FILE(mpif77)
ENDIF(OMPI_WANT_F77_BINDINGS)

#IF(OMPI_WANT_F90_BINDINGS)
#  CONFIGURE_WRAPPER_DATA_FILE(mpif90)
#ENDIF(OMPI_WANT_F90_BINDINGS)

IF(WANT_INSTALL_HEADERS)
  CONFIGURE_WRAPPER_DATA_FILE(opal-wrapper)
ENDIF(WANT_INSTALL_HEADERS)
