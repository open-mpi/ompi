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

IF(WIN32)

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

  IF(WINDOWS_VS)
    SET(OBJ_PATH "${PROJECT_BINARY_DIR}/libopen-pal.dir/${CMAKE_CFG_INTDIR}")
    # for generating the static library, as opal will not export event API any more.
    SET(HWLOC_OBJ_FILES
    ${OBJ_PATH}/${hwloc_dir}_component.obj
    ${OBJ_PATH}/bind.obj
    ${OBJ_PATH}/cpuset.obj
    ${OBJ_PATH}/distances.obj
    ${OBJ_PATH}/dolib.obj
    ${OBJ_PATH}/misc.obj
    ${OBJ_PATH}/topology-libpci.obj
    ${OBJ_PATH}/topology-synthetic.obj
    ${OBJ_PATH}/topology-windows.obj
    ${OBJ_PATH}/topology-x86.obj
    ${OBJ_PATH}/topology-xml.obj
    ${OBJ_PATH}/topology.obj
    ${OBJ_PATH}/traversal.obj
      CACHE INTERNAL "hwloc obj files")
  ELSEIF(WINDOWS_MINGW)
    SET(OBJ_PATH "${PROJECT_BINARY_DIR}/libopen-pal.dir/${CMAKE_CFG_INTDIR}")
    # for generating the static library, as opal will not export event API any more.
    SET(HWLOC_OBJ_FILES
    ${OBJ_PATH}/${hwloc_dir}_component.obj
    ${OBJ_PATH}/bind.obj
    ${OBJ_PATH}/cpuset.obj
    ${OBJ_PATH}/distances.obj
    ${OBJ_PATH}/dolib.obj
    ${OBJ_PATH}/misc.obj
    ${OBJ_PATH}/topology-libpci.obj
    ${OBJ_PATH}/topology-synthetic.obj
    ${OBJ_PATH}/topology-windows.obj
    ${OBJ_PATH}/topology-x86.obj
    ${OBJ_PATH}/topology-xml.obj
    ${OBJ_PATH}/topology.obj
    ${OBJ_PATH}/traversal.obj
      CACHE INTERNAL "hwloc obj files")
  ENDIF(WINDOWS_VS)
  
ELSE(WIN32)
  SET(RESULT_COMPONENT_FILES
    ${RESULT_COMPONENT_FILES}
  )
ENDIF(WIN32)

SET(RESULT TRUE)