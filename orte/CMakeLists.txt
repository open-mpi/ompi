# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



# The source code is compiled as C++ for dynamic build 
# and compiled as C for static build

PROJECT (ORTE)


# Recuresive search sub directories excluding mca, event, include and tools. 
# Add sources in different source groups.
EXECUTE_PROCESS (COMMAND cmd /C dir /AD /B
                 WORKING_DIRECTORY  ${PROJECT_SOURCE_DIR}
                 OUTPUT_VARIABLE    OUTPUT
                 RESULT_VARIABLE    RESULT
                 ERROR_VARIABLE     ERROR)

STRING (REGEX MATCHALL "[a-zA-Z1-9_]+" ORTE_SUBDIRS ${OUTPUT})


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

# Generate flex files.
INCLUDE (find_flex)
ADD_FLEX_FILE(ORTE_SOURCE_FILES "./util/hostfile/hostfile_lex.l" "orte_util_hostfile_"
              "${PROJECT_BINARY_DIR}/util/hostfile/")
SET_SOURCE_FILES_PROPERTIES(${PROJECT_BINARY_DIR}/util/hostfile/hostfile_lex.c
                            PROPERTIES COMPILE_DEFINITIONS YY_NO_UNISTD_H)

INCLUDE (check_mca_subdirs)
SET (ORTE_SOURCE_FILES ${ORTE_SOURCE_FILES} ${MCA_FILES})
SET_SOURCE_FILES_PROPERTIES(${ORTE_SOURCE_FILES} PROPERTIES LANGUAGE CXX)

ADD_LIBRARY (libopen-rte ${ORTE_SOURCE_FILES})

ADD_DEPENDENCIES (libopen-rte libopen-pal)

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rte PROPERTIES COMPILE_FLAGS "-D_USRDLL -DORTE_EXPORTS -DOPAL_IMPORTS")
  TARGET_LINK_LIBRARIES (libopen-rte Ws2_32.lib shlwapi.lib)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rte PROPERTIES COMPILE_FLAGS "-D_LIB")
ENDIF(BUILD_SHARED_LIBS)

# generate orte_config.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orte/include/orte_config.h.in  ${OpenMPI_BINARY_DIR}/orte/include/orte_config.h)

INCLUDE(generate_version_file)

ADD_SUBDIRECTORY(tools)

# Install libraries and shared files.
INSTALL(TARGETS libopen-rte
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES orted/help-orted.txt runtime/help-orte-runtime.txt
  util/hostfile/help-hostfile.txt util/dash_host/help-dash-host.txt
  DESTINATION share/openmpi)
