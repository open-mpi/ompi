# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# The CCP components need to import the type library ccpapi.tlb,
# if it's not installed, the CCP components won't be built.

SET(EXTRA_INCLUDE_PATH "")

IF(NOT CCPAPI_FOUND)
  MESSAGE(STATUS "looking for ccp...")

  IF(CMAKE_CL_64)
    SET(CCP_LIB_PATH $ENV{CCP_LIB64} CACHE PATH "CCP library path" FORCE)
  ELSE(CMAKE_CL_64)
    SET(CCP_LIB_PATH $ENV{CCP_LIB32} CACHE PATH "CCP library path" FORCE)
  ENDIF(CMAKE_CL_64)

  FIND_FILE(CCPAPI_FOUND ccpapi.tlb PATHS ${CCP_LIB_PATH})

  IF(CCPAPI_FOUND)
    SET(RESULT TRUE)
    SET(RESULT_INCLUDE_PATH ${CCP_LIB_PATH})
    MESSAGE(STATUS "looking for ccp...found.")
  ELSE(CCPAPI_FOUND)
    SET(RESULT FALSE)   
    MESSAGE(STATUS "looking for ccp...not found.")
  ENDIF(CCPAPI_FOUND)

ELSE(NOT CCPAPI_FOUND)
  SET(RESULT_INCLUDE_PATH ${CCP_LIB_PATH})
  SET(RESULT TRUE)
ENDIF(NOT CCPAPI_FOUND)
