#
# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


#
#  LIBLTDL_PATH           - path to libltdl dir
#  LIBLTDL_LIB_PATH       - path to ltdl.lib
#  LIBLTDL_INCLUDE_PATH   - path to ltdl.h
#  LIBLTDL_FOUND          - system has Libltdl
#
SET(EXTRA_INCLUDE_PATH "")
SET(LIBLTDL_PATH "" CACHE PATH "Path to libltdl root directory.")

IF(NOT OMPI_WANT_LIBLTDL)
  SET(LIBLTDL_FOUND FALSE CACHE INTERNAL "find result of libltdl.")
ELSE(NOT OMPI_WANT_LIBLTDL)

  IF(NOT LIBLTDL_FOUND OR NOT "${LIBLTDL_PATH}" STREQUAL "${LIBLTDL_OLD_PATH}")
    MESSAGE(STATUS "looking for libltdl...")

    # set the default search path
    IF("${LIBLTDL_PATH}" STREQUAL "")
      SET(LIBLTDL_PATH "$ENV{ProgramFiles}/GnuWin32")
    ENDIF("${LIBLTDL_PATH}" STREQUAL "")

    # clear the old find results and start a new search
    UNSET(LIBLTDL_INCLUDE_PATH CACHE)
    UNSET(LIBLTDL_LIB_PATH CACHE)

    FIND_FILE(LIBLTDL_LIB ltdl.lib PATHS ${LIBLTDL_PATH}/lib)
    FIND_PATH(LIBLTDL_INCLUDE_PATH ltdl.h PATHS ${LIBLTDL_PATH}/include)

    IF(LIBLTDL_LIB AND LIBLTDL_INCLUDE_PATH)
      SET(LIBLTDL_FOUND TRUE CACHE INTERNAL "find result of libltdl.")
      SET(LIBLTDL_OLD_PATH ${LIBLTDL_PATH} CACHE INTERNAL "Store the old libltdl path.")
      MESSAGE(STATUS "looking for libltdl...found.")
    ELSE(LIBLTDL_LIB AND LIBLTDL_INCLUDE_PATH)
      SET(LIBLTDL_FOUND FALSE CACHE INTERNAL "find result of libltdl.")
      MESSAGE(STATUS "looking for libltdl...failed.")
    ENDIF(LIBLTDL_LIB AND LIBLTDL_INCLUDE_PATH)

  ELSE(NOT LIBLTDL_FOUND OR NOT "${LIBLTDL_PATH}" STREQUAL "${LIBLTDL_OLD_PATH}")
    SET(LIBLTDL_FOUND TRUE CACHE INTERNAL "find result of libltdl.")
  ENDIF(NOT LIBLTDL_FOUND OR NOT "${LIBLTDL_PATH}" STREQUAL "${LIBLTDL_OLD_PATH}")

ENDIF(NOT OMPI_WANT_LIBLTDL)
