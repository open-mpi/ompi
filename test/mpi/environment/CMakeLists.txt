# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SET(CHELLO_FILES
  chello.c
)

ADD_EXECUTABLE (chello ${CHELLO_FILES})

TARGET_LINK_LIBRARIES (chello libmpi)
