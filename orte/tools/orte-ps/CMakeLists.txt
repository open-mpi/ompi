# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

PROJECT(ORTE_PS)

FILE(GLOB_RECURSE ORTE_PS_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (orte-ps ${ORTE_PS_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(orte-ps PROPERTIES COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS")
  SET_SOURCE_FILES_PROPERTIES(${ORTE_PS_SOURCE_FILES} PROPERTIES LANGUAGE CXX)
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (orte-ps Ws2_32.lib shlwapi.lib)

ADD_CUSTOM_COMMAND (TARGET orte-ps
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/orte-ps.exe
  ${PROJECT_BINARY_DIR}/ompi-ps.exe
  COMMENT "Copying renamed executables...")

INSTALL(TARGETS orte-ps
  DESTINATION bin)
INSTALL(FILES help-orte-ps.txt DESTINATION share/openmpi)
INSTALL(FILES ${PROJECT_BINARY_DIR}/ompi-ps.exe
  DESTINATION bin)

ADD_DEPENDENCIES (orte-ps libopen-pal libopen-rte)
