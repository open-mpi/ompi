# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

PROJECT(OPAL_CHECKPOINT)

FILE(GLOB_RECURSE OPAL_CHECKPOINT_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (opal-checkpoint ${OPAL_CHECKPOINT_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(opal-checkpoint PROPERTIES COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS")
  SET_SOURCE_FILES_PROPERTIES(${OPAL_CHECKPOINT_SOURCE_FILES} PROPERTIES LANGUAGE CXX)
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (opal-checkpoint Ws2_32.lib shlwapi.lib)

INSTALL(TARGETS opal-checkpoint
  DESTINATION bin)
INSTALL(FILES help-opal-checkpoint.txt DESTINATION share/openmpi)

ADD_DEPENDENCIES (opal-checkpoint libopen-pal)
