# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

INCLUDE (event/WIN32-Code/CMakeLists.txt) # or use INCLUDE (${OpenMPI_SOURCE_DIR}/opal/event/WIN32-Code/CMakeLists.txt)
FOREACH (FILE ${EVENT_WIN32_FILES})
  SET (EVENT_FILES ${EVENT_FILES} WIN32-Code/${FILE})
ENDFOREACH (FILE)

INCLUDE (event/compat/sys/CMakeLists.txt)
FOREACH (FILE ${COMPAT_SYS_FILES})
  SET (EVENT_FILES ${EVENT_FILES} compat/sys/${FILE})
ENDFOREACH (FILE)

IF(WIN32)
  # include special path for these files
  SET_SOURCE_FILES_PROPERTIES(event/event.c event/log.c event/signal.c event/select.c event/evutil.c event/WIN32-Code/win32.c 
    PROPERTIES COMPILE_FLAGS "/I \"${PROJECT_SOURCE_DIR}/event/compat\" /I \"${PROJECT_SOURCE_DIR}/event/WIN32-Code\"")
  SET(EVENT_FILES
    ${EVENT_FILES}
    event-internal.h
    event.h
    event.c
    evsignal.h
    evutil.h
    evutil.c
    log.h
    log.c
    select.c
    signal.c
  )
ELSE(WIN32)
  SET(EVENT_FILES
    ${EVENT_FILES}
  )
ENDIF(WIN32)

