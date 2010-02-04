# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2008      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



# The source code is compiled as C++ for dynamic build 
# and compiled as C for static build

PROJECT (OMPI)

# These two files need to be compiled twice, one time with preprocessor CHECKSUM.
# Make one copy of them with another file name and add them into the project.
CONFIGURE_FILE(datatype/datatype_pack.c ${PROJECT_BINARY_DIR}/datatype/datatype_pack_checksum.c [COPYONLY])
CONFIGURE_FILE(datatype/datatype_unpack.c ${PROJECT_BINARY_DIR}/datatype/datatype_unpack_checksum.c [COPYONLY])

SET(DATATYPE_CHECKSUM_FILES ${PROJECT_BINARY_DIR}/datatype/datatype_pack_checksum.c
                            ${PROJECT_BINARY_DIR}/datatype/datatype_unpack_checksum.c )

SET_SOURCE_FILES_PROPERTIES(${DATATYPE_CHECKSUM_FILES} 
                            PROPERTIES COMPILE_DEFINITIONS CHECKSUM)
SET(OMPI_SOURCE_FILES ${OMPI_SOURCE_FILES} ${DATATYPE_CHECKSUM_FILES})
SOURCE_GROUP(datatype FILES  ${DATATYPE_CHECKSUM_FILES})

# Recuresive search sub directories excluding mca, mpi and tools. 
# Add sources in different source groups.
EXECUTE_PROCESS (COMMAND cmd /C dir /AD /B
                 WORKING_DIRECTORY  ${PROJECT_SOURCE_DIR}
                 OUTPUT_VARIABLE    OUTPUT
                 RESULT_VARIABLE    RESULT
                 ERROR_VARIABLE     ERROR)

STRING (REGEX MATCHALL "[a-zA-Z1-9_]+" OMPI_SUBDIRS ${OUTPUT})

FOREACH(OMPI_SUBDIR ${OMPI_SUBDIRS})

  IF(NOT ${OMPI_SUBDIR} STREQUAL "mca" AND NOT ${OMPI_SUBDIR} STREQUAL "mpi" AND NOT ${OMPI_SUBDIR} STREQUAL "contrib" 
    AND NOT ${OMPI_SUBDIR} STREQUAL "debuggers" AND NOT ${OMPI_SUBDIR} STREQUAL "tools")
      
      FILE(GLOB_RECURSE OMPI_${OMPI_SUBDIR}_FILES 
        "${OMPI_SUBDIR}/*.h" "${OMPI_SUBDIR}/*.c" "${OMPI_SUBDIR}/*.cc" "${OMPI_SUBDIR}/*.cpp")
        
      SET (OMPI_SOURCE_FILES
        ${OMPI_SOURCE_FILES}
        ${OMPI_${OMPI_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${OMPI_SUBDIR}" FILES ${OMPI_${OMPI_SUBDIR}_FILES})
      
  ENDIF(NOT ${OMPI_SUBDIR} STREQUAL "mca" AND NOT ${OMPI_SUBDIR} STREQUAL "mpi" AND NOT ${OMPI_SUBDIR} STREQUAL "contrib" 
    AND NOT ${OMPI_SUBDIR} STREQUAL "debuggers" AND NOT ${OMPI_SUBDIR} STREQUAL "tools")

ENDFOREACH(OMPI_SUBDIR ${OMPI_SUBDIRS})

# Special care should be taken for the debugger directory
SET(OMPI_DEBUGGER_FILES "debuggers/ompi_debuggers.c" "debuggers/debuggers.h")
SOURCE_GROUP("debuggers" FILES ${OMPI_DEBUGGER_FILES})
SET(OMPI_SOURCE_FILES ${OMPI_SOURCE_FILES} ${OMPI_DEBUGGER_FILES})
SET_SOURCE_FILES_PROPERTIES("debuggers/ompi_debuggers.c"
                            PROPERTIES COMPILE_DEFINITIONS "OMPI_MSGQ_DLL=\"\$(pkglibdir)/libompi_dbg_msgq.dll\";OMPI_MSGQ_DLL_PREFIX=\"libompi_dbg_msgq\";OMPI_MPIHANDLES_DLL_PREFIX=\"libompi_dbg_mpihandles\"")

# Add MPI C files
FILE(GLOB OMPI_MPI_C_FILES "mpi/c/*.h" "mpi/c/*.c")

# Remove all MPI_File related files if the option is not selected
IF (NOT OMPI_PROVIDE_MPI_FILE_INTERFACE)
  MESSAGE( STATUS "Skipping the MPI I/O interface")
  SET( TMP_SRC "" )
  FOREACH ( FILENAME ${OMPI_MPI_C_FILES})
    GET_FILENAME_COMPONENT(relname ${FILENAME} NAME)
    IF (NOT ${relname} MATCHES "file.*[ch]$")
      IF (NOT ${relname} STREQUAL "register_datarep.c")
        LIST(APPEND TMP_SRC ${FILENAME})
      ENDIF (NOT ${relname} STREQUAL "register_datarep.c")
    ENDIF(NOT ${relname} MATCHES "file.*[ch]$")
  ENDFOREACH(FILENAME)
  SET( OMPI_MPI_C_FILES ${TMP_SRC})
ENDIF(NOT OMPI_PROVIDE_MPI_FILE_INTERFACE)

SET (OMPI_SOURCE_FILES ${OMPI_SOURCE_FILES} ${OMPI_MPI_C_FILES})
SOURCE_GROUP(mpi "")
SOURCE_GROUP(mpi\\c FILES ${OMPI_MPI_C_FILES})

IF(OMPI_ENABLE_MPI_PROFILING)
  # As weak symbols are not supported by MS compiler,
  # we have to compile the C source files again for profiler,
  # i.e. add the pre-processor "OMPI_PROFILING_DEFINES" explicitly.
  
  # first get the file names.
  FILE(GLOB OMPI_PROFILE_NAMES "mpi/c/*.c" )
  # then copy them to the build directory with a prefix.
  FOREACH(FILE_NAME ${OMPI_MPI_C_FILES})
    GET_FILENAME_COMPONENT(relname ${FILE_NAME} NAME)
    IF(NOT ${relname} STREQUAL "attr_fn.c")
      CONFIGURE_FILE(${FILE_NAME}
                     ${PROJECT_BINARY_DIR}/mpi/c/profile/p${relname} [COPYONLY])
    ENDIF(NOT ${relname} STREQUAL "attr_fn.c")
  ENDFOREACH(FILE_NAME ${OMPI_MPI_C_FILES})

  FILE(GLOB_RECURSE OMPI_C_PROFILE_FILES "${PROJECT_BINARY_DIR}/mpi/c/profile/*.c" )

  SET_SOURCE_FILES_PROPERTIES(${OMPI_C_PROFILE_FILES} 
                              PROPERTIES COMPILE_DEFINITIONS OMPI_PROFILING_DEFINES)
  SET(OMPI_SOURCE_FILES ${OMPI_SOURCE_FILES} ${OMPI_C_PROFILE_FILES})
  SOURCE_GROUP(mpi\\c\\profile FILES  ${OMPI_C_PROFILE_FILES})
ELSE(OMPI_ENABLE_MPI_PROFILING)
  MESSAGE( STATUS "Skipping the MPI profiling interface")
ENDIF(OMPI_ENABLE_MPI_PROFILING)


IF(OMPI_WANT_F77_BINDINGS)
  # A handful of files in mpi/f77/base must be included in libmpi, in order to build the
  # Fortran 77 glue into libmpi
  FILE(GLOB OMPI_F77_BASE_FILES "mpi/f77/base/*.c")
  SET_SOURCE_FILES_PROPERTIES(${OMPI_F77_BASE_FILES} 
                              PROPERTIES COMPILE_FLAGS "-DOMPI_COMPILING_F77_WRAPPERS=1 -DOMPI_PROFILE_LAYER=0")
  SET(OMPI_SOURCE_FILES ${OMPI_SOURCE_FILES} ${OMPI_F77_BASE_FILES})
  SOURCE_GROUP(mpi\\f77\\base FILES ${OMPI_F77_BASE_FILES})
ENDIF(OMPI_WANT_F77_BINDINGS)


INCLUDE (check_mca_subdirs)
SET (OMPI_SOURCE_FILES ${OMPI_SOURCE_FILES} ${MCA_FILES})

ADD_LIBRARY (libmpi ${OMPI_SOURCE_FILES})

SET_TARGET_PROPERTIES(libmpi PROPERTIES COMPILE_FLAGS 
                      "-DOMPI_MPIHANDLES_DLL_PREFIX=libompi_dbg_mpihandles
                       -DOMPI_MSGQ_DLL_PREFIX=libompi_dbg_msgq
                       -DOMPI_BUILDING 
                       -DOMPI_BUILDING_CXX_BINDINGS_LIBRARY")

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libmpi PROPERTIES 
    COMPILE_FLAGS "-D_USRDLL -DHAVE_CONFIG_H -DOMPI_EXPORTS /TP")
  TARGET_LINK_LIBRARIES (libmpi libopen-pal libopen-rte Ws2_32.lib shlwapi.lib)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libmpi PROPERTIES COMPILE_FLAGS "-D_LIB")
ENDIF(BUILD_SHARED_LIBS)

# generate ompi_config.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/ompi/include/ompi_config.h.in  ${OpenMPI_BINARY_DIR}/ompi/include/ompi_config.h)

INCLUDE(generate_version_file)

ADD_SUBDIRECTORY(mpi)
ADD_SUBDIRECTORY(tools)


# Install libraries headers, and shared files
INSTALL(TARGETS libmpi
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES ${PROJECT_BINARY_DIR}/include/mpi.h DESTINATION include)

INSTALL(FILES errhandler/help-mpi-errors.txt
  DESTINATION share/openmpi)

IF (OMPI_DEBUG_BUILD)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libmpi${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD)

IF(OMPI_WANT_F77_BINDINGS)
  INSTALL(FILES ${PROJECT_BINARY_DIR}/include/mpif.h
    ${PROJECT_BINARY_DIR}/include/mpif-config.h 
    ${PROJECT_SOURCE_DIR}/include/mpif-common.h
    ${PROJECT_SOURCE_DIR}/include/mpif-mpi-io.h
    DESTINATION include)
ENDIF(OMPI_WANT_F77_BINDINGS)

INSTALL(FILES ${PROJECT_SOURCE_DIR}/runtime/help-mpi-runtime.txt ${PROJECT_SOURCE_DIR}/mpi/help-mpi-api.txt
  DESTINATION share/openmpi)
INSTALL(DIRECTORY mpi/cxx/ DESTINATION include/openmpi/ompi/mpi/cxx
  FILES_MATCHING PATTERN "*.h" PATTERN ".svn" EXCLUDE)

