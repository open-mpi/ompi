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
#  LIBLTDL_PATH  - path to ltdl.h
#  LIBLTDL_FOUND - system has Libltdl
#

MACRO(FIND_LIBLTDL)
  
  IF(NOT LIBLTDL_FOUND)
    MESSAGE(STATUS "Looking for libltdl...")

    FIND_PATH(LIBLTDL_PATH ltdl.h PATHS "$ENV{ProgramFiles}/GnuWin32/share/libtool/libltdl" ${LIBLTDL_PATH})

    IF (WIN32)
      IF(LIBLTDL_PATH)
        MESSAGE(STATUS "Looking for libltdl...found")
        SET(LIBLTDL_FOUND TRUE TRUE CACHE INTERNAL "found libltdl")
      ELSE(LIBLTDL_PATH)
        MESSAGE(STATUS "Looking for libltdl...not found. Skip.")
        SET(LIBLTDL_FOUND FALSE)
      ENDIF(LIBLTDL_PATH)
    ELSE(WIN32)
      # nothing to do here at moment.
    ENDIF(WIN32)
    
  ENDIF(NOT LIBLTDL_FOUND)

ENDMACRO(FIND_LIBLTDL)
