# Copyright (c) 2012      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


MESSAGE(STATUS "configure hwloc.")

STRING(REGEX MATCH "hwloc[0-9]+" hwloc_dir "${CURRENT_PATH}")

INCLUDE_DIRECTORIES ("${CURRENT_PATH}/"
    "${CURRENT_PATH}/../"
    "${CURRENT_PATH}/hwloc/include/")

SET(HWLOC_PATH ${CURRENT_PATH} CACHE INERNAL "path to hwloc dir")

SET(RESULT_COMPONENT_FILES
    ${RESULT_COMPONENT_FILES}
    ${CURRENT_PATH}/${hwloc_dir}_component.c
    ${CURRENT_PATH}/hwloc/src/bind.c
    ${CURRENT_PATH}/hwloc/src/cpuset.c
    ${CURRENT_PATH}/hwloc/src/distances.c
    ${CURRENT_PATH}/hwloc/src/dolib.c
    ${CURRENT_PATH}/hwloc/src/misc.c
    ${CURRENT_PATH}/hwloc/src/topology-libpci.c
    ${CURRENT_PATH}/hwloc/src/topology-synthetic.c
    ${CURRENT_PATH}/hwloc/src/topology-windows.c
    ${CURRENT_PATH}/hwloc/src/topology-x86.c
    ${CURRENT_PATH}/hwloc/src/topology-xml.c
    ${CURRENT_PATH}/hwloc/src/topology.c
    ${CURRENT_PATH}/hwloc/src/traversal.c
  )

SET(RESULT TRUE)