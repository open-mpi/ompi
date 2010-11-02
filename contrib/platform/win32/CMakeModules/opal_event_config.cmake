# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Only one libevent component should be used, selection is done by editing .windows in_use property.

FILE(STRINGS ${CURRENT_PATH}/.windows IN_USE REGEX "^in_use=")

IF(IN_USE STREQUAL "0")

  SET(RESULT FALSE)

ELSE(IN_USE STREQUAL "0")

  IF(LIBEVENT_CONFIG_DONE)
    MESSAGE(STATUS "multiple libevent selected, only one is configured.")
    SET(RESULT FALSE)

  ELSE(LIBEVENT_CONFIG_DONE)
    SET(LIBEVENT_FOUND TRUE CACHE INTERNAL "allow only one event mca.")

    MESSAGE(STATUS "configure libevent.")

    # set up event include directories.
    INCLUDE_DIRECTORIES ("${CURRENT_PATH}/libevent/compat"
      "${CURRENT_PATH}/libevent/WIN32-Code/"
      "${CURRENT_PATH}/libevent/include/"
      "${CURRENT_PATH}/libevent"
      "${PROJECT_BINARY_DIR}/mca/event/libevent207/libevent/include/")

    SET(LIBEVENT_INCLUDE_DIRS ${CURRENT_PATH}/libevent/compat;${CURRENT_PATH}/libevent/WIN32-Code/;${CURRENT_PATH}/libevent/include/;${CURRENT_PATH}/libevent;${PROJECT_BINARY_DIR}/mca/event/libevent207/libevent/include/
    CACHE INTERNAL "the libevent dirs that have to be included on the top level.")
      
    IF(WIN32)

      # generating config.h
      # windows doesn't need this file, just make an empty one
      FILE(WRITE ${PROJECT_BINARY_DIR}/mca/event/libevent207/libevent/include/config.h
        " /* config.h.  Generated automatically by CMake. */ ")

      SET(RESULT_SOURCE_FILES
        ${RESULT_SOURCE_FILES}
        ${CURRENT_PATH}/libevent207_component.c
        ${CURRENT_PATH}/libevent207_module.c
        #system sources
        ${CURRENT_PATH}/libevent/win32select.c
        ${CURRENT_PATH}/libevent/evthread_win32.c
        ${CURRENT_PATH}/libevent/buffer_iocp.c
        ${CURRENT_PATH}/libevent/event_iocp.c
        ${CURRENT_PATH}/libevent/bufferevent_async.c
        #core sources
        ${CURRENT_PATH}/libevent/event.c
        ${CURRENT_PATH}/libevent/evthread.c
        ${CURRENT_PATH}/libevent/buffer.c
        ${CURRENT_PATH}/libevent/bufferevent.c
        ${CURRENT_PATH}/libevent/bufferevent_sock.c
        ${CURRENT_PATH}/libevent/bufferevent_filter.c
        ${CURRENT_PATH}/libevent/bufferevent_pair.c
        ${CURRENT_PATH}/libevent/listener.c
        ${CURRENT_PATH}/libevent/bufferevent_ratelim.c
	    ${CURRENT_PATH}/libevent/evmap.c
        ${CURRENT_PATH}/libevent/log.c
        ${CURRENT_PATH}/libevent/evutil.c
        ${CURRENT_PATH}/libevent/evutil_rand.c
        ${CURRENT_PATH}/libevent/strlcpy.c
        ${CURRENT_PATH}/libevent/signal.c
        ${CURRENT_PATH}/libevent/event_tagging.c
      )

      OMPI_DEF(OPAL_HAVE_WORKING_EVENTOPS 1 
        "Whether our event component has working event operations or not if not, then assumedly it only has working timers and signals)." 0 1)

      OMPI_DEF(MCA_event_IMPLEMENTATION_HEADER "${CURRENT_PATH}/libevent207.h"
        "Header to include for event implementation" 1 1)

    ELSE(WIN32)
      SET(RESULT_SOURCE_FILES
        ${RESULT_SOURCE_FILES}
      )
    ENDIF(WIN32)

  SET(RESULT TRUE)

  ENDIF(LIBEVENT_CONFIG_DONE)

ENDIF(IN_USE STREQUAL "0")