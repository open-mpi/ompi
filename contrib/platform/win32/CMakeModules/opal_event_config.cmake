# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

MESSAGE(STATUS "configure libevent.")

# set up event include directories.
INCLUDE_DIRECTORIES ("${CURRENT_PATH}/libevent/compat"
  "${CURRENT_PATH}/libevent/WIN32-Code/"
  "${CURRENT_PATH}/libevent/include/"
  "${CURRENT_PATH}/libevent"
  "${PROJECT_BINARY_DIR}/mca/event/libevent207/libevent/include/")

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
ELSE(WIN32)
  SET(RESULT_SOURCE_FILES
    ${RESULT_SOURCE_FILES}
  )
ENDIF(WIN32)

SET(RESULT TRUE)