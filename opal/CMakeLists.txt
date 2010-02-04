# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


PROJECT (OPAL)


# Add include sub directory to gernerate opal_config.h
ADD_SUBDIRECTORY (include)


# Add source files

# Windows event files
INCLUDE (event/CMakeLists.txt)
FOREACH (FILE ${EVENT_FILES})
  SET (OPAL_EVENT_FILES ${OPAL_EVENT_FILES} ${PROJECT_SOURCE_DIR}/event/${FILE})
ENDFOREACH (FILE)
SOURCE_GROUP(event FILES ${OPAL_EVENT_FILES})

# Include headers.
FILE(GLOB OPAL_INCLUDE_FILES "include/*.h" "include/opal/*.h" "include/opal/sys/win32/*.h")
SOURCE_GROUP(include FILES ${OPAL_INCLUDE_FILES})

SET (OPAL_SOURCE_FILES
  ${OPAL_SOURCE_FILES}
  ${OPAL_EVENT_FILES}
  ${OPAL_INCLUDE_FILES}
)

# Recuresive search sub directories excluding mca, event, include and tools. 
# Add sources in different source groups.
EXECUTE_PROCESS (COMMAND cmd /C dir /AD /B
                 WORKING_DIRECTORY  ${PROJECT_SOURCE_DIR}
                 OUTPUT_VARIABLE    OUTPUT
                 RESULT_VARIABLE    RESULT
                 ERROR_VARIABLE     ERROR)

STRING (REGEX MATCHALL "[a-zA-Z1-9_]+" OPAL_SUBDIRS ${OUTPUT})


FOREACH(OPAL_SUBDIR ${OPAL_SUBDIRS})

  IF(NOT ${OPAL_SUBDIR} STREQUAL "mca" AND NOT ${OPAL_SUBDIR} STREQUAL "event" 
    AND NOT ${OPAL_SUBDIR} STREQUAL "libltdl" AND NOT ${OPAL_SUBDIR} STREQUAL "include" 
    AND NOT ${OPAL_SUBDIR} STREQUAL "tools")
      
      IF(NOT ${OPAL_SUBDIR} STREQUAL "win32")
        FILE(GLOB_RECURSE OPAL_${OPAL_SUBDIR}_FILES 
          "${OPAL_SUBDIR}/*.h" "${OPAL_SUBDIR}/*.c" "${OPAL_SUBDIR}/*.cc" "${OPAL_SUBDIR}/*.cpp")
      ELSE(NOT ${OPAL_SUBDIR} STREQUAL "win32")
        FILE(GLOB OPAL_${OPAL_SUBDIR}_FILES 
          "${OPAL_SUBDIR}/*.h" "${OPAL_SUBDIR}/*.c" "${OPAL_SUBDIR}/*.cc" "${OPAL_SUBDIR}/*.cpp")
      ENDIF(NOT ${OPAL_SUBDIR} STREQUAL "win32")
      
      SET (OPAL_SOURCE_FILES
        ${OPAL_SOURCE_FILES}
        ${OPAL_${OPAL_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${OPAL_SUBDIR}" FILES ${OPAL_${OPAL_SUBDIR}_FILES})
      
  ENDIF(NOT ${OPAL_SUBDIR} STREQUAL "mca" AND NOT ${OPAL_SUBDIR} STREQUAL "event"
    AND NOT ${OPAL_SUBDIR} STREQUAL "libltdl" AND NOT ${OPAL_SUBDIR} STREQUAL "include"
    AND NOT ${OPAL_SUBDIR} STREQUAL "tools")

ENDFOREACH(OPAL_SUBDIR ${OPAL_SUBDIRS})

#only generate if it's not a tarball
IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c) 
  ADD_FLEX_FILE(OPAL_SOURCE_FILES ./util/keyval/keyval_lex.l 
    opal_util_keyval_yy "${PROJECT_BINARY_DIR}/util/keyval/") 
  SET_SOURCE_FILES_PROPERTIES(${PROJECT_BINARY_DIR}/util/keyval/keyval_lex.c
    PROPERTIES COMPILE_DEFINITIONS YY_NO_UNISTD_H)
ELSE(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c)
  SET_SOURCE_FILES_PROPERTIES(${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c
    PROPERTIES COMPILE_DEFINITIONS YY_NO_UNISTD_H)
ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c)

IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/show_help_lex.c) 
  ADD_FLEX_FILE(OPAL_SOURCE_FILES ./util/show_help_lex.l 
    opal_show_help_yy "${PROJECT_BINARY_DIR}/util/")
  SET_SOURCE_FILES_PROPERTIES(${PROJECT_BINARY_DIR}/util/show_help_lex.c
    PROPERTIES COMPILE_DEFINITIONS YY_NO_UNISTD_H)
ELSE(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/show_help_lex.c)
  SET_SOURCE_FILES_PROPERTIES(${PROJECT_SOURCE_DIR}/util/show_help_lex.c
    PROPERTIES COMPILE_DEFINITIONS YY_NO_UNISTD_H)
ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/show_help_lex.c) 


# Add source files from mca subdirectories.
INCLUDE (check_mca_subdirs)
SET (OPAL_SOURCE_FILES ${OPAL_SOURCE_FILES} ${MCA_FILES})


ADD_LIBRARY (libopen-pal ${OPAL_SOURCE_FILES})

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-pal PROPERTIES COMPILE_FLAGS "-D_USRDLL -DOPAL_EXPORTS")
  IF(OMPI_WANT_LIBLTDL AND LIBLTDL_FOUND) 
    INCLUDE_DIRECTORIES(${LIBLTDL_INCLUDE_PATH}) 
    TARGET_LINK_LIBRARIES (libopen-pal Ws2_32.lib shlwapi.lib ${LIBLTDL_LIB}) 
  ELSE(OMPI_WANT_LIBLTDL AND LIBLTDL_FOUND) 
    TARGET_LINK_LIBRARIES (libopen-pal Ws2_32.lib shlwapi.lib) 
  ENDIF(OMPI_WANT_LIBLTDL AND LIBLTDL_FOUND)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-pal PROPERTIES COMPILE_FLAGS "-D_LIB")
ENDIF(BUILD_SHARED_LIBS)

# Generate install_dirs.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/contrib/platform/win32/ConfigFiles/install_dirs.h.cmake
  ${OpenMPI_BINARY_DIR}/opal/mca/installdirs/config/install_dirs.h)

# Generate version file.
INCLUDE(generate_version_file)

ADD_SUBDIRECTORY(tools)

# Install libraries, shared files.
INSTALL(TARGETS libopen-pal
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES runtime/help-opal-runtime.txt
    mca/base/help-mca-base.txt
    mca/base/help-mca-param.txt
    DESTINATION share/openmpi)

IF (OMPI_DEBUG_BUILD)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libopen-pal${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD)
