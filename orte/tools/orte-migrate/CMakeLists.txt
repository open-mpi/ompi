# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2009-2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE ORTE_MIGRATE_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (orte-migrate ${ORTE_MIGRATE_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(orte-migrate PROPERTIES
    COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS /TP")
ENDIF(BUILD_SHARED_LIBS)

SET_TARGET_PROPERTIES(orte-migrate PROPERTIES
  DEBUG_POSTFIX "${OMPI_EXE_DEBUG_POSTFIX}")

TARGET_LINK_LIBRARIES (orte-migrate Ws2_32.lib shlwapi.lib)

ADD_DEPENDENCIES (orte-migrate libopen-pal libopen-rte)

ADD_CUSTOM_COMMAND (TARGET orte-migrate
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/orte-migrate.exe
  ${PROJECT_BINARY_DIR}/ompi-migrate.exe
  COMMENT "Copying renamed executables...")

INSTALL(TARGETS orte-migrate
  DESTINATION bin)
INSTALL(FILES help-orte-migrate.txt DESTINATION share/openmpi)
INSTALL(FILES ${PROJECT_BINARY_DIR}/ompi-migrate.exe
  DESTINATION bin)
