# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OPAL_CHECKPOINT_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (opal-checkpoint ${OPAL_CHECKPOINT_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(opal-checkpoint PROPERTIES 
    COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS /TP")
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (opal-checkpoint libopen-pal Ws2_32.lib shlwapi.lib)

INSTALL(TARGETS opal-checkpoint
  DESTINATION bin)
INSTALL(FILES help-opal-checkpoint.txt DESTINATION share/openmpi)

