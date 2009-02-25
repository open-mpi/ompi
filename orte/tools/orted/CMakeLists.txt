# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE ORTED_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (orted ${ORTED_SOURCE_FILES})

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(orted PROPERTIES COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS")
  SET_SOURCE_FILES_PROPERTIES(${ORTED_SOURCE_FILES} PROPERTIES LANGUAGE CXX)
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (orted Ws2_32.lib shlwapi.lib)

INSTALL(TARGETS orted
  DESTINATION bin)

ADD_DEPENDENCIES (orted libopen-pal libopen-rte)

