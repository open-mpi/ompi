# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


PROJECT (ORTE)


# Recuresive search sub directories excluding mca, event, include and tools. 
# Add sources in different source groups.
INCLUDE(list_subdirs)
CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}" ORTE_SUBDIRS)


FOREACH(ORTE_SUBDIR ${ORTE_SUBDIRS})

  IF(NOT ${ORTE_SUBDIR} STREQUAL "mca" AND NOT ${ORTE_SUBDIR} STREQUAL "tools"
    AND NOT ${ORTE_SUBDIR} STREQUAL "test")
      
      FILE(GLOB_RECURSE ORTE_${ORTE_SUBDIR}_FILES 
        "${ORTE_SUBDIR}/*.h" "${ORTE_SUBDIR}/*.c" "${ORTE_SUBDIR}/*.cc" "${ORTE_SUBDIR}/*.cpp")
                  
      SET (ORTE_SOURCE_FILES
        ${ORTE_SOURCE_FILES}
        ${ORTE_${ORTE_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${ORTE_SUBDIR}" FILES ${ORTE_${ORTE_SUBDIR}_FILES})
      
  ENDIF(NOT ${ORTE_SUBDIR} STREQUAL "mca" AND NOT ${ORTE_SUBDIR} STREQUAL "tools"
    AND NOT ${ORTE_SUBDIR} STREQUAL "test")

ENDFOREACH(ORTE_SUBDIR ${ORTE_SUBDIRS})

#only generate if it's not a tarball
IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/hostfile/hostfile_lex.c)
  ADD_FLEX_FILE(ORTE_SOURCE_FILES ./util/hostfile/hostfile_lex.l "orte_util_hostfile_"
    "${PROJECT_BINARY_DIR}/util/hostfile/")
ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/hostfile/hostfile_lex.c)


INCLUDE (check_mca_subdirs)
SET (ORTE_SOURCE_FILES ${ORTE_SOURCE_FILES} ${MCA_FILES})

ADD_LIBRARY (libopen-rte ${ORTE_SOURCE_FILES})


# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rte PROPERTIES
    COMPILE_FLAGS "-D_USRDLL -DORTE_EXPORTS -DOPAL_IMPORTS /TP")
  TARGET_LINK_LIBRARIES (libopen-rte libopen-pal Ws2_32.lib shlwapi.lib)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rte PROPERTIES COMPILE_FLAGS "-D_LIB /TP")
ENDIF(BUILD_SHARED_LIBS)

# generate orte_config.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orte/include/orte_config.h.in  ${OpenMPI_BINARY_DIR}/orte/include/orte_config.h)

# generate version.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orte/include/orte/version.h.in  ${OpenMPI_BINARY_DIR}/orte/include/orte/version.h)

ADD_SUBDIRECTORY(tools)

# Install libraries and shared files.
INSTALL(TARGETS libopen-rte
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES orted/help-orted.txt runtime/help-orte-runtime.txt
  util/hostfile/help-hostfile.txt util/dash_host/help-dash-host.txt
  mca/odls/default/help-odls-default.txt
  DESTINATION share/openmpi)

IF (OMPI_DEBUG_BUILD)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libopen-rte${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD)

