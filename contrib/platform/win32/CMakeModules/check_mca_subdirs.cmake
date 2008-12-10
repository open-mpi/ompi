#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# list the sub directories of current directories
# save the list of subdirs in OUTPUT_VARIABLE

MACRO(CHECK_MCA_SUBDIRS CURRENT_DIR OUTPUT_VARIABLE)

  EXECUTE_PROCESS (COMMAND cmd /C dir /AD /B
                   WORKING_DIRECTORY  ${CURRENT_DIR}
                   OUTPUT_VARIABLE    OUTPUT
                   RESULT_VARIABLE    RESULT
                   ERROR_VARIABLE     ERROR)

  IF(NOT "${OUTPUT}" STREQUAL "")
    STRING (REGEX MATCHALL "[a-zA-Z1-9_]+" ${OUTPUT_VARIABLE} ${OUTPUT})
  ENDIF(NOT "${OUTPUT}" STREQUAL "")

ENDMACRO(CHECK_MCA_SUBDIRS CURRENT_DIR OUTPUT_VARIABLE)


CHECK_MCA_SUBDIRS("${PROJECT_SOURCE_DIR}/mca" "${PROJECT_NAME}_MCA_SUBDIRS")

FILE(GLOB ${PROJECT_NAME}_MCA_HEADER_FILES "mca/*.C" "mca/*.h")
SET(${PROJECT_NAME}_MCA_FILES ${${PROJECT_NAME}_MCA_FILES} ${${PROJECT_NAME}_MCA_HEADER_FILES})
SOURCE_GROUP(mca FILES ${${PROJECT_NAME}_MCA_HEADER_FILES})

# parse each mca subdir
FOREACH ("${PROJECT_NAME}_MCA_SUBDIR" ${${PROJECT_NAME}_MCA_SUBDIRS})

  IF(NOT ${${PROJECT_NAME}_MCA_SUBDIR} STREQUAL "CMakeFiles" AND NOT ${${PROJECT_NAME}_MCA_SUBDIR} STREQUAL "svn")

    SET(CURRENT_PATH "mca/${${PROJECT_NAME}_MCA_SUBDIR}")
    FILE(GLOB ${${PROJECT_NAME}_MCA_SUBDIR}_FILES "${CURRENT_PATH}/*.C" "${CURRENT_PATH}/*.h"
                                                  "${CURRENT_PATH}/*.cc" "${CURRENT_PATH}/*.cpp")
    SET(${PROJECT_NAME}_MCA_FILES ${${PROJECT_NAME}_MCA_FILES} ${${${PROJECT_NAME}_MCA_SUBDIR}_FILES})
    SOURCE_GROUP(mca\\${${PROJECT_NAME}_MCA_SUBDIR} FILES ${${${PROJECT_NAME}_MCA_SUBDIR}_FILES})
    
    IF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/base")
      SET(CURRENT_PATH "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/base")
      FILE(GLOB ${${PROJECT_NAME}_MCA_SUBDIR}_BASE_FILES "${CURRENT_PATH}/*.c" "${CURRENT_PATH}/*.h"
                                                         "${CURRENT_PATH}/*.cc" "${CURRENT_PATH}/*.cpp")
      SET_SOURCE_FILES_PROPERTIES(${PROJECT_BINARY_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/base/static-components.h 
        PROPERTIES GENERATED true)
      SET(${${PROJECT_NAME}_MCA_SUBDIR}_BASE_FILES ${${${PROJECT_NAME}_MCA_SUBDIR}_BASE_FILES}
        ${PROJECT_BINARY_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/base/static-components.h)
      SET(${PROJECT_NAME}_MCA_FILES ${${PROJECT_NAME}_MCA_FILES} 
        ${${${PROJECT_NAME}_MCA_SUBDIR}_BASE_FILES})
      SOURCE_GROUP(mca\\${${PROJECT_NAME}_MCA_SUBDIR}\\base FILES ${${${PROJECT_NAME}_MCA_SUBDIR}_BASE_FILES})
      
      # Install help files if they are here.
      INSTALL(DIRECTORY ${CURRENT_PATH}/ DESTINATION share/openmpi/
        FILES_MATCHING PATTERN "*.txt" PATTERN ".svn" EXCLUDE)
      
    ENDIF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/base")

    CHECK_MCA_SUBDIRS("${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}" "${PROJECT_NAME}_MCA_COMPONENT_SUBDIRS")

    SET(CURRENT_COMPONENT_PRIORITY 0)
    SET(BEST_COMPONENT_PRIORITY 0)
    # parse each component subdir of current mca
    FOREACH (${PROJECT_NAME}_MCA_COMPONENT ${${PROJECT_NAME}_MCA_COMPONENT_SUBDIRS})

      IF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/.windows")
        FILE(STRINGS ${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/.windows
             VALUE REGEX "^not_single_shared_lib=")
        IF(NOT VALUE STREQUAL "")
          STRING(REPLACE "not_single_shared_lib=" "" NOT_SINGLE_SHARED_LIB ${VALUE})
        ENDIF(NOT VALUE STREQUAL "")
        
        IF(NOT BUILD_SHARED_LIBS OR NOT_SINGLE_SHARED_LIB STREQUAL "1")
          SET(NOT_SINGLE_SHARED_LIB "")
          # add sources for static build or for the shared build when this is not a stand along library.
          SET(CURRENT_PATH "mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}")
          FILE(GLOB ${${PROJECT_NAME}_MCA_COMPONENT}_FILES "${CURRENT_PATH}/*.C" "${CURRENT_PATH}/*.h"
                                                           "${CURRENT_PATH}/*.cc" "${CURRENT_PATH}/*.cpp")
          SET(${PROJECT_NAME}_MCA_FILES ${${PROJECT_NAME}_MCA_FILES} 
            ${${${PROJECT_NAME}_MCA_COMPONENT}_FILES})
          SOURCE_GROUP(mca\\${${PROJECT_NAME}_MCA_SUBDIR}\\${${PROJECT_NAME}_MCA_COMPONENT} FILES ${${${PROJECT_NAME}_MCA_COMPONENT}_FILES})

          IF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/configure.params")
            FILE(STRINGS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/configure.params" 
              CURRENT_COMPONENT_PRIORITY REGEX "PRIORITY")
            IF(NOT CURRENT_COMPONENT_PRIORITY STREQUAL "")
              STRING(REGEX REPLACE "[A-Z_]+=" "" CURRENT_COMPONENT_PRIORITY ${CURRENT_COMPONENT_PRIORITY})
            ENDIF(NOT CURRENT_COMPONENT_PRIORITY STREQUAL "")
          ENDIF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/configure.params")
          
          IF(CURRENT_COMPONENT_PRIORITY GREATER BEST_COMPONENT_PRIORITY)
            # I have a higher priority for this mca, put me at the very beginning.
            SET (OUTFILE_EXTERN
              "extern const mca_base_component_t mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_component"  
              "\n${OUTFILE_EXTERN}")
            SET(${${PROJECT_NAME}_MCA_SUBDIR}_STRUCT
              "&mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_component,\n"  
              ${${${PROJECT_NAME}_MCA_SUBDIR}_STRUCT})
            SET(BEST_COMPONENT_PRIORITY ${CURRENT_COMPONENT_PRIORITY})
          ELSE(CURRENT_COMPONENT_PRIORITY GREATER BEST_COMPONENT_PRIORITY)
            SET (OUTFILE_EXTERN ${OUTFILE_EXTERN}
              "\nextern const mca_base_component_t mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_component;")
            SET(${${PROJECT_NAME}_MCA_SUBDIR}_STRUCT ${${${PROJECT_NAME}_MCA_SUBDIR}_STRUCT}
              "&mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_component,\n")
          ENDIF(CURRENT_COMPONENT_PRIORITY GREATER BEST_COMPONENT_PRIORITY)
        ELSE(NOT BUILD_SHARED_LIBS OR NOT_SINGLE_SHARED_LIB STREQUAL "1")
          SET(CURRENT_PATH ${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT})
 
          # get the dependencies for this component.
          SET(MCA_DEPENDENCIES "")
          FILE(STRINGS ${CURRENT_PATH}/.windows VALUE REGEX "^mca_dependencies=")
          IF(NOT VALUE STREQUAL "")
            STRING(REPLACE "mca_dependencies=" "" MCA_DEPENDENCIES ${VALUE})
          ENDIF(NOT VALUE STREQUAL "")

          # get the libraries required for this component.
          SET(MCA_LINK_LIBRARIES "")
          FILE(STRINGS ${CURRENT_PATH}/.windows VALUE REGEX "^mca_link_libraries=")
          IF(NOT VALUE STREQUAL "")
            STRING(REPLACE "mca_link_libraries=" "" MCA_LINK_LIBRARIES ${VALUE})
          ENDIF(NOT VALUE STREQUAL "")

          # generate CMakeLists.txt for each component for shared build.
          FILE (WRITE "${PROJECT_BINARY_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/CMakeLists.txt"
            "
#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# make new project for shared build
PROJECT(\"mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}\")

FILE(GLOB ${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_FILES
     \"${CURRENT_PATH}/*.C\" 
     \"${CURRENT_PATH}/*.h\" 
     \"${CURRENT_PATH}/*.cc\" 
     \"${CURRENT_PATH}/*.cpp\")

SET_SOURCE_FILES_PROPERTIES(\${${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_FILES}
                            PROPERTIES LANGUAGE CXX)

ADD_LIBRARY(mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT} SHARED 
            \${${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}_FILES})

SET_TARGET_PROPERTIES(mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT}
                      PROPERTIES COMPILE_FLAGS \"-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS\")

TARGET_LINK_LIBRARIES (mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT} ${MCA_LINK_LIBRARIES})

ADD_DEPENDENCIES(mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT} libopen-pal ${MCA_DEPENDENCIES})

INSTALL(TARGETS mca_${${PROJECT_NAME}_MCA_SUBDIR}_${${PROJECT_NAME}_MCA_COMPONENT} DESTINATION lib/openmpi)
          ")
          
          ADD_SUBDIRECTORY (${PROJECT_BINARY_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT} mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT})
        ENDIF(NOT BUILD_SHARED_LIBS OR NOT_SINGLE_SHARED_LIB STREQUAL "1")

        # Install help files if they are here.
        INSTALL(DIRECTORY ${CURRENT_PATH}/ DESTINATION share/openmpi/
          FILES_MATCHING PATTERN "*.txt" PATTERN ".svn" EXCLUDE)
        
      ENDIF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/${${PROJECT_NAME}_MCA_COMPONENT}/.windows")
    ENDFOREACH (${PROJECT_NAME}_MCA_COMPONENT)
    STRING(LENGTH "${${${PROJECT_NAME}_MCA_SUBDIR}_STRUCT}" STRUCT_STRING_LENTH)
    IF(STRUCT_STRING_LENTH GREATER 0)
      STRING (REPLACE ";" "" OUTFILE_STRUCT ${${${PROJECT_NAME}_MCA_SUBDIR}_STRUCT})
    ENDIF(STRUCT_STRING_LENTH GREATER 0)
    # write out static-component.h for this mca.
    FILE(WRITE "${PROJECT_BINARY_DIR}/mca/${${PROJECT_NAME}_MCA_SUBDIR}/base/static-components.h" 
      "/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
  extern \"C\" {
#endif

${OUTFILE_EXTERN}

const mca_base_component_t *mca_${${PROJECT_NAME}_MCA_SUBDIR}_base_static_components[] = {
  ${OUTFILE_STRUCT}
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
  ")

  SET (OUTFILE_EXTERN "")
  SET (OUTFILE_STRUCT "")
  ENDIF(NOT ${${PROJECT_NAME}_MCA_SUBDIR} STREQUAL "CMakeFiles" AND NOT ${${PROJECT_NAME}_MCA_SUBDIR} STREQUAL "svn")
ENDFOREACH (${PROJECT_NAME}_MCA_SUBDIR)
