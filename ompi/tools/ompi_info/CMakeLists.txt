# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OMPI_INFO_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (ompi_info ${OMPI_INFO_SOURCE_FILES})

ADD_DEPENDENCIES(ompi_info libmpi)
IF(BUILD_SHARED_LIBS)
  TARGET_LINK_LIBRARIES (ompi_info ${OpenMPI_BINARY_DIR}/libopen-pal-lt${CMAKE_DEBUG_POSTFIX}.lib
    ${OpenMPI_BINARY_DIR}/libopen-rte-lt${CMAKE_DEBUG_POSTFIX}.lib
    libmpi Ws2_32.lib shlwapi.lib)
  SET_TARGET_PROPERTIES(ompi_info PROPERTIES
     COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS
                    ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS
                    ${OMPI_CXX_LAN_FLAG}")
ELSE(BUILD_SHARED_LIBS)
  TARGET_LINK_LIBRARIES (ompi_info libmpi libopen-rte libopen-pal Ws2_32.lib shlwapi.lib)
ENDIF(BUILD_SHARED_LIBS)


INSTALL(TARGETS ompi_info
  DESTINATION bin)
INSTALL(FILES help-ompi_info.txt DESTINATION share/openmpi)
