# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


PROJECT (ORCA)


# Recuresive search sub directories excluding mca, event, include and tools. 
# Add sources in different source groups.
INCLUDE(list_subdirs)
CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}" ORCA_SUBDIRS)

SET(ORCA_EXCLUDE_SUBDIRS mca tools)

FOREACH(ORCA_SUBDIR ${ORCA_SUBDIRS})

  LIST(FIND ORCA_EXCLUDE_SUBDIRS ${ORCA_SUBDIR} ORCA_EXCLUDE_SUBDIR)

  IF(${ORCA_EXCLUDE_SUBDIR} EQUAL -1)
      
      FILE(GLOB_RECURSE ORCA_${ORCA_SUBDIR}_FILES 
        "${ORCA_SUBDIR}/*.h" "${ORCA_SUBDIR}/*.c" "${ORCA_SUBDIR}/*.cc" "${ORCA_SUBDIR}/*.cpp")
                  
      SET (ORCA_SOURCE_FILES
        ${ORCA_SOURCE_FILES}
        ${ORCA_${ORCA_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${ORCA_SUBDIR}" FILES ${ORCA_${ORCA_SUBDIR}_FILES})
      
  ENDIF(${ORCA_EXCLUDE_SUBDIR} EQUAL -1)

ENDFOREACH(ORCA_SUBDIR ${ORCA_SUBDIRS})


#only generate if it's not a tarball


INCLUDE (check_mca_subdirs)
SET (ORCA_SOURCE_FILES ${ORCA_SOURCE_FILES} ${MCA_FILES})

ADD_LIBRARY (libopen-rca ${ORCA_SOURCE_FILES})

ADD_DEPENDENCIES(libopen-rca libopen-pal)

IF(WINDOWS_MINGW)
  SET(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIRARIES} -lpthread")
  SET(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIRARIES} -lpthread")
ENDIF(WINDOWS_MINGW)

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rca PROPERTIES COMPILE_DEFINITIONS "_USRDLL;ORCA_EXPORTS;OPAL_IMPORTS" 
                                    COMPILE_FLAGS "${OMPI_CXX_LAN_FLAG}" PREFIX "" IMPORT_PREFIX "")

  IF(WINDOWS_MINGW)
    TARGET_LINK_LIBRARIES(libopen-rca ${OpenMPI_BINARY_DIR}/event${CMAKE_DEBUG_POSTFIX}.lib
      libopen-pal Ws2_32.lib shlwapi.lib)
  ELSEIF(WINDOWS_VS)
    TARGET_LINK_LIBRARIES(libopen-rca ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/event${CMAKE_DEBUG_POSTFIX}.lib
      libopen-pal Ws2_32.lib shlwapi.lib)
  ENDIF(WINDOWS_MINGW)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rca PROPERTIES COMPILE_DEFINITIONS "${OMPI_C_DEF_PRE}_LIB"
                                    COMPILE_FLAGS "${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

# generate orca_config.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orca/include/orca_config.h.in  ${OpenMPI_BINARY_DIR}/orca/include/orca_config.h)

# generate version.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orca/include/orca/version.h.in  ${OpenMPI_BINARY_DIR}/orca/include/orca/version.h)

ADD_SUBDIRECTORY(tools)

# Install libraries and shared files.
INSTALL(TARGETS libopen-rca
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES runtime/help-orca-runtime.txt
  DESTINATION share/openmpi)

IF (OMPI_DEBUG_BUILD AND WINDOWS_VS)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libopen-rca${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD AND WINDOWS_VS)
