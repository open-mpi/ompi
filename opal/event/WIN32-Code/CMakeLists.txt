# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


IF(WIN32)
  SET (EVENT_WIN32_FILES
    ${EVENT_WIN32_FILES}
    config.h
    misc.h
    misc.h
    tree.h
    win32.c
  )
ELSE(WIN32)
ENDIF(WIN32)
