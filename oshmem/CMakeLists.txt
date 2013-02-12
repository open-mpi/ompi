# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



# The source code is compiled as C++ for dynamic build 
# and compiled as C for static build

PROJECT (OSHMEM)

# Recuresive search sub directories excluding mca, mpi and tools. 
# Add sources in different source groups.
INCLUDE(list_subdirs)
CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}" OSHMEM_SUBDIRS)


SET(OSHMEM_EXCLUDE_SUBDIRS mca shmem class info)

FOREACH(OSHMEM_SUBDIR ${OSHMEM_SUBDIRS})

  LIST(FIND OSHMEM_EXCLUDE_SUBDIRS ${OSHMEM_SUBDIR} OSHMEM_EXCLUDE_SUBDIR)

  IF(${OSHMEM_EXCLUDE_SUBDIR} EQUAL -1)

      FILE(GLOB_RECURSE OSHMEM_${OSHMEM_SUBDIR}_FILES 
        "${OSHMEM_SUBDIR}/*.h" "${OSHMEM_SUBDIR}/*.c" "${OSHMEM_SUBDIR}/*.cc" "${OSHMEM_SUBDIR}/*.cpp")
        
      SET (OSHMEM_SOURCE_FILES
        ${OSHMEM_SOURCE_FILES}
        ${OSHMEM_${OSHMEM_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${OSHMEM_SUBDIR}" FILES ${OSHMEM_${OSHMEM_SUBDIR}_FILES})
      
  ENDIF(${OSHMEM_EXCLUDE_SUBDIR} EQUAL  -1)
  
ENDFOREACH(OSHMEM_SUBDIR ${OSHMEM_SUBDIRS})


# Add MPI C files
FILE(GLOB OSHMEM_SHMEM_C_FILES "shmem/c/*.h" "shmem/c/*.c")


SET (OSHMEM_SOURCE_FILES ${OSHMEM_SOURCE_FILES} ${OSHMEM_SHMEM_C_FILES})
SOURCE_GROUP(shmem"")
SOURCE_GROUP(shmem\\c FILES ${OSHMEM_SHMEM_C_FILES})



IF(OMPI_WANT_F77_BINDINGS)
  # A handful of files in mpi/f77/base must be included in libshmem, in order to build the
  # Fortran 77 glue into libshmem
  FILE(GLOB OMPI_F77_BASE_FILES "mpi/f77/base/*.c")
  SET_SOURCE_FILES_PROPERTIES(${OSHMEM_F77_BASE_FILES} 
                              PROPERTIES COMPILE_FLAGS "-DOMPI_COMPILING_F77_WRAPPERS=1 -DOMPI_PROFILE_LAYER=0")
  SET(OSHMEM_SOURCE_FILES ${OSHMEM_SOURCE_FILES} ${OMPI_F77_BASE_FILES})
  SOURCE_GROUP(mpi\\f77\\base FILES ${OMPI_F77_BASE_FILES})
ENDIF(OMPI_WANT_F77_BINDINGS)


INCLUDE (check_mca_subdirs)
SET (OSHMEM_SOURCE_FILES ${OSHMEM_SOURCE_FILES} ${MCA_FILES})

ADD_LIBRARY (libshmem ${OSHMEM_SOURCE_FILES})

SET_TARGET_PROPERTIES(libshmem PROPERTIES COMPILE_FLAGS 
                      "${OSHMEM_C_DEF_PRE}OMPI_MPIHANDLES_DLL_PREFIX=liboshmem_dbg_mpihandles
                       ${OSHMEM_C_DEF_PRE}OMPI_MSGQ_DLL_PREFIX=libompi_dbg_msgq
                       ${OSHMEM_C_DEF_PRE}OMPI_BUILDING 
                       ${OSHMEM_C_DEF_PRE}OMPI_BUILDING_CXX_BINDINGS_LIBRARY")

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  ADD_DEPENDENCIES (libshmem libopen-pal libopen-rte)
  SET_TARGET_PROPERTIES(libshmem PROPERTIES 
    COMPILE_FLAGS "-D_USRDLL -DHAVE_CONFIG_H -DOSHMEM_EXPORTS /TP")
  TARGET_LINK_LIBRARIES (libshmem Ws2_32.lib shlwapi.lib)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libshmem PROPERTIES COMPILE_FLAGS "-D_LIB")
ENDIF(BUILD_SHARED_LIBS)

# generate shmem.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/oshmem/include/shmem.h.in ${OpenMPI_BINARY_DIR}/oshmem/include/shmem.h)

# generate oshmem_config.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/oshmem/include/oshmem_config.h.in  ${OpenMPI_BINARY_DIR}/oshmem/include/oshmem_config.h)

# generate shmem_portable_platform.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/oshmem/include/shmem_portable_platform.h.in  ${OpenMPI_BINARY_DIR}/oshmem/include/shmem_portable_platform.h)

# generate version.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/oshmem/include/oshmem/version.h.in  ${OpenMPI_BINARY_DIR}/oshmem/include/oshmem/version.h)

ADD_SUBDIRECTORY(shmem)


# Install libraries headers, and shared files
INSTALL(TARGETS libshmem
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES ${PROJECT_BINARY_DIR}/include/shmem.h ${PROJECT_BINARY_DIR}/include/shmem_portable_platform.h
  DESTINATION include)

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

INSTALL(FILES ${PROJECT_SOURCE_DIR}/runtime/help-shmem-runtime.txt ${PROJECT_SOURCE_DIR}/shmem/help-shmem-api.txt
  DESTINATION share/openmpi)
INSTALL(DIRECTORY shmem/cxx/ DESTINATION include/openshmem/oshmem/shmem/cxx
  FILES_MATCHING PATTERN "*.h" PATTERN ".svn" EXCLUDE)

