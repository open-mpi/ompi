# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE ORTE_CHECKPOINT_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (orte-checkpoint ${ORTE_CHECKPOINT_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(orte-checkpoint PROPERTIES
    COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS /TP")
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (orte-checkpoint libopen-pal libopen-rte Ws2_32.lib shlwapi.lib)

ADD_CUSTOM_COMMAND (TARGET orte-checkpoint
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/orte-checkpoint.exe
  ${PROJECT_BINARY_DIR}/ompi-checkpoint.exe
  COMMENT "Copying renamed executables...")

INSTALL(TARGETS orte-checkpoint
  DESTINATION bin)
INSTALL(FILES help-orte-checkpoint.txt DESTINATION share/openmpi)
INSTALL(FILES ${PROJECT_BINARY_DIR}/ompi-checkpoint.exe
  DESTINATION bin)
