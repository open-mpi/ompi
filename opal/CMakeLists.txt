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

# Generate flex files.
ADD_FLEX_FILE(OPAL_SOURCE_FILES ./util/keyval/keyval_lex.l opal_util_keyval_yy "${PROJECT_BINARY_DIR}/util/keyval/")
ADD_FLEX_FILE(OPAL_SOURCE_FILES ./util/show_help_lex.l  opal_show_help_yy "${PROJECT_BINARY_DIR}/util/")

# Exclude the same flex files that generaged on Linux,
# which could be found in a tarball.
IF(EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c)
  LIST(REMOVE_ITEM OPAL_SOURCE_FILES "${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c")
ENDIF(EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c)

IF(EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/show_help_lex.c)
  LIST(REMOVE_ITEM OPAL_SOURCE_FILES "${PROJECT_SOURCE_DIR}/util/keyval/show_help_lex.c")
ENDIF(EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/show_help_lex.c)

# Add Libtool support
IF(OMPI_WANT_LIBLTDL)
  # if we want libltdl, that means ltdl.h and ltdl.c has been found,
  # just copy them into the source tree for compiling.
  CONFIGURE_FILE("${LIBLTDL_PATH}/ltdl.c" ${CMAKE_CURRENT_BINARY_DIR}/libltdl/ltdl.c COPYONLY)
  CONFIGURE_FILE("${LIBLTDL_PATH}/ltdl.h" ${CMAKE_CURRENT_BINARY_DIR}/libltdl/ltdl.h COPYONLY)

  # insert "#include opal_config.h" into ltdl.c, otherwise, it won't compile.
  FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/libltdl/ltdl.c LTDL_C_PART1 LIMIT 1183)
  FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/libltdl/ltdl.c LTDL_C_PART2 OFFSET 1183)
  FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/libltdl/ltdl.c
    "${LTDL_C_PART1}" "\n#include \"opal_config.h\"\n" "${LTDL_C_PART2}")

  SET (LIBLTDL_FILES libltdl/ltdl.c libltdl/ltdl.h)
  SET_SOURCE_FILES_PROPERTIES(${LIBLTDL_FILES} PROPERTIES COMPILE_DEFINITIONS 
                              "error_t=int;HAVE_ERRNO_H;LTDL_OBJDIR=\".\";LTDL_SHLIB_EXT=\".dll\"")
  SET (OPAL_SOURCE_FILES ${OPAL_SOURCE_FILES} ${LIBLTDL_FILES})
  SOURCE_GROUP(libltdl FILES ${LIBLTDL_FILES})
ENDIF(OMPI_WANT_LIBLTDL)

# Add source files from mca subdirectories.
INCLUDE (check_mca_subdirs)
SET (OPAL_SOURCE_FILES ${OPAL_SOURCE_FILES} ${MCA_FILES})


ADD_LIBRARY (libopen-pal ${OPAL_SOURCE_FILES})

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_SOURCE_FILES_PROPERTIES(${OPAL_SOURCE_FILES} 
                              PROPERTIES LANGUAGE C)

# Flex is trying to include the unistd.h file. As there is no configure
# option on this, the flex generated files will try to include the file
# even on platforms without unistd.h (such as Windows). Therefore, if we
# know this file is not available, we can prevent flex from including it.
# Thus, define "YY_NO_UNISTD_H" as the target property (the same for ORTE).
  SET_TARGET_PROPERTIES(libopen-pal PROPERTIES COMPILE_FLAGS "-D_USRDLL -DOPAL_EXPORTS")
  TARGET_LINK_LIBRARIES (libopen-pal Ws2_32.lib shlwapi.lib)
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
INSTALL(FILES runtime/help-opal-runtime.txt DESTINATION share/openmpi)
