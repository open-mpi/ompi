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

ADD_DEFINITIONS(\"-DOMPI_CONFIGURE_DATE=\\\"${OMPI_CONFIGURE_DATE}\\\"\"
                \"-DOMPI_BUILD_DATE=\\\"${OMPI_BUILD_DATE}\\\"\")

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(ompi_info PROPERTIES 
    COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS /TP")
ENDIF(BUILD_SHARED_LIBS)

TARGET_LINK_LIBRARIES (ompi_info libmpi libopen-pal libopen-rte Ws2_32.lib shlwapi.lib)
INSTALL(TARGETS ompi_info
  DESTINATION bin)
INSTALL(FILES help-ompi_info.txt DESTINATION share/openmpi)

