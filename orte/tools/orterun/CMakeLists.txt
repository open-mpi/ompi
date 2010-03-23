# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE ORTERUN_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (orterun ${ORTERUN_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(orterun PROPERTIES 
    COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS /TP")
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (orterun libopen-pal libopen-rte Ws2_32.lib shlwapi.lib)

ADD_CUSTOM_COMMAND (TARGET orterun
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/orterun.exe
  ${PROJECT_BINARY_DIR}/mpirun.exe
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/orterun.exe
  ${PROJECT_BINARY_DIR}/mpiexec.exe
  COMMENT "Copying renamed executables...")

INSTALL(TARGETS orterun
  DESTINATION bin)
INSTALL(FILES help-orterun.txt DESTINATION share/openmpi)
INSTALL(FILES ${PROJECT_BINARY_DIR}/mpirun.exe
  DESTINATION bin)
INSTALL(FILES ${PROJECT_BINARY_DIR}/mpiexec.exe
  DESTINATION bin)

