#
# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# list the sub directories of current directories
# save the list of subdirs in OUTPUT_VARIABLE

MACRO(CHECK_SUBDIRS CURRENT_DIR OUTPUT_VARIABLE)

  EXECUTE_PROCESS (COMMAND cmd /C dir /AD /B
                   WORKING_DIRECTORY  ${CURRENT_DIR}
                   OUTPUT_VARIABLE    OUTPUT
                   RESULT_VARIABLE    RESULT
                   ERROR_VARIABLE     ERROR)

  IF(NOT "${OUTPUT}" STREQUAL "")
    STRING (REGEX MATCHALL "[a-zA-Z1-9_]+" ${OUTPUT_VARIABLE} ${OUTPUT})
  ENDIF(NOT "${OUTPUT}" STREQUAL "")

ENDMACRO(CHECK_SUBDIRS CURRENT_DIR OUTPUT_VARIABLE)

SET(MCA_FRAMEWORK_LIST "")
CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}/mca" MCA_FRAMEWORK_LIST)
#MESSAGE("MCA_FRAMEWORK_LIST:${MCA_FRAMEWORK_LIST}")


FILE(GLOB ${PROJECT_NAME}_MCA_HEADER_FILES "mca/*.C" "mca/*.h")
SET(${PROJECT_NAME}_MCA_FILES ${${PROJECT_NAME}_MCA_FILES} ${${PROJECT_NAME}_MCA_HEADER_FILES})
SOURCE_GROUP(mca FILES ${${PROJECT_NAME}_MCA_HEADER_FILES})

# clear the variables first
SET(MCA_FRAMEWORK "")
SET(MCA_FRAMEWORK_FILES "")
SET(MCA_FILES "")

# parse each mca subdir
FOREACH (MCA_FRAMEWORK ${MCA_FRAMEWORK_LIST})

  IF(NOT ${MCA_FRAMEWORK} STREQUAL "CMakeFiles" AND NOT ${MCA_FRAMEWORK} STREQUAL "svn")
    #SET(CURRENT_PATH "mca/${${PROJECT_NAME}_MCA_SUBDIR}")
    FILE(GLOB MCA_FRAMEWORK_FILES "mca/${MCA_FRAMEWORK}/*.C" "mca/${MCA_FRAMEWORK}/*.h"
                                  "mca/${MCA_FRAMEWORK}/*.cc" "mca/${MCA_FRAMEWORK}/*.cpp")
    SET(MCA_FILES ${MCA_FILES} ${MCA_FRAMEWORK_FILES})
    SOURCE_GROUP(mca\\${MCA_FRAMEWORK} FILES ${MCA_FRAMEWORK_FILES})
    
    SET(COMPONENT_LIST "")
    CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}/mca/${MCA_FRAMEWORK}" COMPONENT_LIST)

    SET(CURRENT_COMPONENT_PRIORITY 0)
    SET(BEST_COMPONENT_PRIORITY 0)

    # parse each component subdir of current mca framework
    FOREACH (MCA_COMPONENT ${COMPONENT_LIST})

      IF(${MCA_COMPONENT} STREQUAL "base")

        SET(CURRENT_PATH "${PROJECT_SOURCE_DIR}/mca/${MCA_FRAMEWORK}/base")
        FILE(GLOB MCA_FRAMEWORK_BASE_FILES "${CURRENT_PATH}/*.c" "${CURRENT_PATH}/*.h"
                                           "${CURRENT_PATH}/*.cc" "${CURRENT_PATH}/*.cpp")

        IF(EXISTS "${CURRENT_PATH}/.windows")

          #MESSAGE("MCA_FRAMEWORK_BASE_FILES:${MCA_FRAMEWORK_BASE_FILES}")
          SET(EXCLUDE_LIST "")
          FILE(STRINGS ${CURRENT_PATH}/.windows EXCLUDE_LIST REGEX "^exclude_list=")

          IF(NOT EXCLUDE_LIST STREQUAL "")
            STRING(REPLACE "exclude_list=" "" EXCLUDE_LIST ${EXCLUDE_LIST})
          ENDIF(NOT EXCLUDE_LIST STREQUAL "")

          # remove the files in the exclude list
          FOREACH(FILE ${EXCLUDE_LIST})
            LIST(REMOVE_ITEM MCA_FRAMEWORK_BASE_FILES "${CURRENT_PATH}/${FILE}")
          ENDFOREACH(FILE)

        ENDIF(EXISTS "${CURRENT_PATH}/.windows")

        SET_SOURCE_FILES_PROPERTIES(${PROJECT_BINARY_DIR}/mca/${MCA_FRAMEWORK}/base/static-components.h 
          PROPERTIES GENERATED true)
        SET(MCA_FRAMEWORK_BASE_FILES ${MCA_FRAMEWORK_BASE_FILES}
          ${PROJECT_BINARY_DIR}/mca/${MCA_FRAMEWORK}/base/static-components.h)
        SET(MCA_FILES ${MCA_FILES} ${MCA_FRAMEWORK_BASE_FILES})

        SOURCE_GROUP(mca\\${MCA_FRAMEWORK}\\base FILES ${MCA_FRAMEWORK_BASE_FILES})

        # Install help files if they are here.
        INSTALL(DIRECTORY ${CURRENT_PATH}/ DESTINATION share/openmpi/
          FILES_MATCHING PATTERN "*.txt" PATTERN ".svn" EXCLUDE)

      ELSEIF(EXISTS "${PROJECT_SOURCE_DIR}/mca/${MCA_FRAMEWORK}/${MCA_COMPONENT}/.windows")

        SET(COMPONENT_FILES "")
        SET(CURRENT_PATH ${PROJECT_SOURCE_DIR}/mca/${MCA_FRAMEWORK}/${MCA_COMPONENT})
        FILE(GLOB COMPONENT_FILES "${CURRENT_PATH}/*.C" "${CURRENT_PATH}/*.h"
          "${CURRENT_PATH}/*.cc" "${CURRENT_PATH}/*.cpp")

        # by default, build this component.
        SET(BUILD_COMPONENT TRUE)

        # do we have to run a check module first?
        SET(REQUIRED_CHECK "")
        FILE(STRINGS ${CURRENT_PATH}/.windows REQUIRED_CHECK REGEX "^required_check=")
        
        SET(EXTRA_INCLUDE_PATH "")
        IF(NOT REQUIRED_CHECK STREQUAL "")
          STRING(REPLACE "required_check=" "" REQUIRED_CHECK ${REQUIRED_CHECK})
          INCLUDE(${REQUIRED_CHECK})
          IF(RESULT)
            SET(EXTRA_INCLUDE_PATH ${RESULT_INCLUDE_PATH})
          ELSE(RESULT)
            # Required check failed, don't build this component.
            SET(BUILD_COMPONENT FALSE)
          ENDIF(RESULT)
        ENDIF(NOT REQUIRED_CHECK STREQUAL "")

        IF(BUILD_COMPONENT)

          # check out if we have to exlude some source files.
          SET(EXCLUDE_LIST "")
          FILE(STRINGS ${CURRENT_PATH}/.windows EXCLUDE_LIST REGEX "^exclude_list=")
        
          IF(NOT EXCLUDE_LIST STREQUAL "")
            STRING(REPLACE "exclude_list=" "" EXCLUDE_LIST ${EXCLUDE_LIST})
          ENDIF(NOT EXCLUDE_LIST STREQUAL "")
        
          # remove the files in the exclude list
          FOREACH(FILE ${EXCLUDE_LIST})
            LIST(REMOVE_ITEM COMPONENT_FILES "${CURRENT_PATH}/${FILE}")
          ENDFOREACH(FILE)

          # add sources for static build or for the shared build when this is not a stand along library.
          SET(MCA_FILES ${MCA_FILES} ${COMPONENT_FILES})
          SOURCE_GROUP(mca\\${MCA_FRAMEWORK}\\${MCA_COMPONENT} FILES ${COMPONENT_FILES})

          INCLUDE_DIRECTORIES(${EXTRA_INCLUDE_PATH})

          IF(EXISTS "${CURRENT_PATH}/configure.params")
            FILE(STRINGS "${CURRENT_PATH}/configure.params" 
              CURRENT_COMPONENT_PRIORITY REGEX "PRIORITY")
            IF(NOT CURRENT_COMPONENT_PRIORITY STREQUAL "")
              STRING(REGEX REPLACE "[A-Z_]+=" "" CURRENT_COMPONENT_PRIORITY ${CURRENT_COMPONENT_PRIORITY})
            ENDIF(NOT CURRENT_COMPONENT_PRIORITY STREQUAL "")
          ENDIF(EXISTS "${CURRENT_PATH}/configure.params")
          
          IF(CURRENT_COMPONENT_PRIORITY GREATER BEST_COMPONENT_PRIORITY)
            # I have a higher priority for this mca, put me at the very beginning.
            SET (OUTFILE_EXTERN
              "extern const mca_base_component_t mca_${MCA_FRAMEWORK}_${MCA_COMPONENT}_component"  
              "\n${OUTFILE_EXTERN}")
            SET(FRAMEWORK_STRUCT_DEF
              "&mca_${MCA_FRAMEWORK}_${MCA_COMPONENT}_component,\n"  
              ${FRAMEWORK_STRUCT_DEF})
            SET(BEST_COMPONENT_PRIORITY ${CURRENT_COMPONENT_PRIORITY})
          ELSE(CURRENT_COMPONENT_PRIORITY GREATER BEST_COMPONENT_PRIORITY)
            SET (OUTFILE_EXTERN ${OUTFILE_EXTERN}
              "\nextern const mca_base_component_t mca_${MCA_FRAMEWORK}_${MCA_COMPONENT}_component;")
            SET(FRAMEWORK_STRUCT_DEF ${FRAMEWORK_STRUCT_DEF}
              "&mca_${MCA_FRAMEWORK}_${MCA_COMPONENT}_component,\n")
          ENDIF(CURRENT_COMPONENT_PRIORITY GREATER BEST_COMPONENT_PRIORITY)

          # Install help files if they are here.
          INSTALL(DIRECTORY ${CURRENT_PATH}/ DESTINATION share/openmpi/
            FILES_MATCHING PATTERN "*.txt" PATTERN ".svn" EXCLUDE)

        ENDIF(BUILD_COMPONENT)        
      ENDIF(${MCA_COMPONENT} STREQUAL "base")

    ENDFOREACH(MCA_COMPONENT)

    STRING(LENGTH "${FRAMEWORK_STRUCT_DEF}" STRUCT_STRING_LENTH)
    IF(STRUCT_STRING_LENTH GREATER 0)
      STRING (REPLACE ";" "" OUTFILE_STRUCT ${FRAMEWORK_STRUCT_DEF})
    ENDIF(STRUCT_STRING_LENTH GREATER 0)
    # write out static-component.h for this mca.
    FILE(WRITE "${PROJECT_BINARY_DIR}/mca/${MCA_FRAMEWORK}/base/static-components.h" 
      "/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
  extern \"C\" {
#endif

${OUTFILE_EXTERN}

const mca_base_component_t *mca_${MCA_FRAMEWORK}_base_static_components[] = {
  ${OUTFILE_STRUCT}
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
  ")

  SET(OUTFILE_EXTERN "")
  SET(OUTFILE_STRUCT "")
  SET(FRAMEWORK_STRUCT_DEF "")
  ENDIF(NOT ${MCA_FRAMEWORK} STREQUAL "CMakeFiles" AND NOT ${MCA_FRAMEWORK} STREQUAL "svn")
ENDFOREACH (MCA_FRAMEWORK)
