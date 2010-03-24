# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB_RECURSE OPAL_WRAPPER_SOURCE_FILES "*.h" "*.c" "*.cc" "*.cpp")

ADD_EXECUTABLE (opal-wrapper ${OPAL_WRAPPER_SOURCE_FILES})

TARGET_LINK_LIBRARIES (opal-wrapper libopen-pal Ws2_32.lib shlwapi.lib)

IF(BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(opal-wrapper PROPERTIES
    COMPILE_FLAGS "-D_USRDLL -DOPAL_IMPORTS -DOMPI_IMPORTS -DORTE_IMPORTS /TP")
ENDIF(BUILD_SHARED_LIBS)

ADD_CUSTOM_COMMAND (TARGET opal-wrapper
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper.exe
  ${PROJECT_BINARY_DIR}/mpicc.exe
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper.exe
  ${PROJECT_BINARY_DIR}/mpic++.exe
  COMMAND ${CMAKE_COMMAND} -E copy
  ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper.exe
  ${PROJECT_BINARY_DIR}/mpicxx.exe
  COMMENT "Copying renamed executables...")
  
INSTALL(TARGETS opal-wrapper
  DESTINATION bin)
INSTALL(FILES help-opal-wrapper.txt
  DESTINATION share/openmpi
  RENAME help-opal-wrapper.exe.txt)
INSTALL(FILES ${PROJECT_BINARY_DIR}/mpicc.exe
  ${PROJECT_BINARY_DIR}/mpic++.exe
  ${PROJECT_BINARY_DIR}/mpicxx.exe
  DESTINATION bin)

IF(WANT_INSTALL_HEADERS)

  ADD_CUSTOM_COMMAND (TARGET opal-wrapper
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper.exe
    ${PROJECT_BINARY_DIR}/opalcc.exe
    COMMENT "Copying renamed executables...")

  IF(OMPI_WANT_CXX_BINDINGS)
    ADD_CUSTOM_COMMAND (TARGET opal-wrapper
      POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy
      ${OpenMPI_BINARY_DIR}/${CMAKE_CFG_INTDIR}/opal-wrapper.exe
      ${PROJECT_BINARY_DIR}/opalc++.exe
      COMMENT "Copying renamed executables...")

    CONFIGURE_FILE(${OPAL_SOURCE_DIR}/tools/wrappers/opalc++-wrapper-data.txt.in
                   ${OPAL_BINARY_DIR}/tools/wrappers/opalc++-wrapper-data.txt @ONLY)
    INSTALL(FILES ${OPAL_BINARY_DIR}/tools/wrappers/opalc++-wrapper-data.txt
            DESTINATION share/openmpi)
  ENDIF(OMPI_WANT_CXX_BINDINGS)
  
  CONFIGURE_FILE(${OPAL_SOURCE_DIR}/tools/wrappers/opalcc-wrapper-data.txt.in
                 ${OPAL_BINARY_DIR}/tools/wrappers/opalcc-wrapper-data.txt @ONLY)
  INSTALL(FILES ${OPAL_BINARY_DIR}/tools/wrappers/opalcc-wrapper-data.txt
          DESTINATION share/openmpi)


ENDIF(WANT_INSTALL_HEADERS)
