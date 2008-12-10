# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


PROJECT(CHELLO)
SET(CHELLO_FILES
  chello.c
)

ADD_EXECUTABLE (chello ${CHELLO_FILES})

TARGET_LINK_LIBRARIES (chello Ws2_32.lib shlwapi.lib)

ADD_DEPENDENCIES (chello libopen-pal libopen-rte libmpi)

