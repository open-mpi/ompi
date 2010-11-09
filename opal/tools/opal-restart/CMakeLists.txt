# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OPAL_RESTART_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (opal-restart ${OPAL_RESTART_SOURCE_FILES})

ADD_DEPENDENCIES(opal-restart libopen-pal)
IF(BUILD_SHARED_LIBS)
  TARGET_LINK_LIBRARIES (opal-restart Ws2_32.lib shlwapi.lib
    ${OpenMPI_BINARY_DIR}/libopen-pal-lt${CMAKE_DEBUG_POSTFIX}.lib)
  SET_TARGET_PROPERTIES(opal-restart PROPERTIES
    COMPILE_FLAGS "${OMPI_C_DEF_PRE}_USRDLL ${OMPI_C_DEF_PRE}OPAL_IMPORTS
                   ${OMPI_C_DEF_PRE}OMPI_IMPORTS ${OMPI_C_DEF_PRE}ORTE_IMPORTS
                   ${OMPI_CXX_LAN_FLAG}")
ELSE(BUILD_SHARED_LIBS)
  TARGET_LINK_LIBRARIES (opal-restart libopen-pal Ws2_32.lib shlwapi.lib)
ENDIF(BUILD_SHARED_LIBS)

INSTALL(TARGETS opal-restart
  DESTINATION bin)
INSTALL(FILES help-opal-restart.txt DESTINATION share/openmpi)
