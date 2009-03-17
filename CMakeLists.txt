#
# Copyright (c) 2007-2008 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



# Check the CMake version
IF(WIN32)
    CMAKE_MINIMUM_REQUIRED(VERSION 2.4.6 FATAL_ERROR)
ELSE(WIN32)
    MESSAGE( FATAL_ERROR "Only support Windows. Aborting.")
ENDIF(WIN32)


# create top-level target(solution file)
PROJECT("OpenMPI" C CXX)


SET (OpenMPI_VERSION $(OpenMPI_MAJOR_VERSION).$(OpenMPI_MINOR_VERSION).$(OpenMPI_PATCH_VERSION))
SET (CMAKE_MODULE_PATH "${OpenMPI_SOURCE_DIR}/contrib/platform/win32/CMakeModules;${CMAKE_MODULE_PATH}") 

# default to debug-builds
IF (NOT CMAKE_BUILD_TYPE)
  SET (CMAKE_BUILD_TYPE Debug CACHE STRING 
    "Choose the type of build, options are: None, Debug, Release, RelWithDebInfo and MinSizeRel." FORCE)
ENDIF (NOT CMAKE_BUILD_TYPE)

# preset install dir
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/installed" CACHE PATH "CMAKE_INSTALL_PREFIX" FORCE)
ENDIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

INCLUDE(ompi_get_version)

# Make the include path visible to all targets
INCLUDE_DIRECTORIES (
  ${OpenMPI_SOURCE_DIR}/
  ${OpenMPI_SOURCE_DIR}/opal
  ${OpenMPI_SOURCE_DIR}/opal/include
  ${OpenMPI_SOURCE_DIR}/ompi
  ${OpenMPI_SOURCE_DIR}/ompi/include
  ${OpenMPI_SOURCE_DIR}/orte
  ${OpenMPI_SOURCE_DIR}/orte/include
  ${OpenMPI_BINARY_DIR}/
  ${OpenMPI_BINARY_DIR}/opal
  ${OpenMPI_BINARY_DIR}/opal/include
  ${OpenMPI_BINARY_DIR}/ompi
  ${OpenMPI_BINARY_DIR}/ompi/include
  ${OpenMPI_BINARY_DIR}/orte
  ${OpenMPI_BINARY_DIR}/orte/include
  ${OpenMPI_SOURCE_DIR}/contrib/platform/win32
)


# Output directroies for binaries and libraries.
SET (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OpenMPI_BINARY_DIR})
SET (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${OpenMPI_BINARY_DIR})
SET (CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${OpenMPI_BINARY_DIR})

# get rid of VS-project file reloading after "cleanup" or "rebuild" 
SET (CMAKE_SUPPRESS_REGENERATION true) 

# Dynamic/shared build, the default is set to static(OFF)
OPTION(BUILD_SHARED_LIBS "Wether we want to build shared libraries." OFF)

INCLUDE(ompi_configure)

ADD_SUBDIRECTORY(opal)
ADD_SUBDIRECTORY(ompi)
ADD_SUBDIRECTORY(orte)
ADD_SUBDIRECTORY(test)

INCLUDE(InstallRequiredSystemLibraries) 

# CPACK settings
#SET(CPACK_PACKAGE_NAME "Open MPI")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Open MPI")
SET(CPACK_PACKAGE_VENDOR "Open MPI")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")

SET(CPACK_PACKAGE_VERSION_MAJOR "${MAJOR_VERSION}")
SET(CPACK_PACKAGE_VERSION_MINOR "${MINOR_VERSION}")
SET(CPACK_PACKAGE_VERSION_PATCH "${RELEASE_VERSION}")

IF(${RELEASE_VERSION} EQUAL 0)
  SET(OpenMPI_version "${MAJOR_VERSION}.${MINOR_VERSION}")
ELSE(${RELEASE_VERSION} EQUAL 0)
  SET(OpenMPI_version "${MAJOR_VERSION}.${MINOR_VERSION}.${RELEASE_VERSION}")
ENDIF(${RELEASE_VERSION} EQUAL 0)

SET(CPACK_PACKAGE_INSTALL_DIRECTORY "OpenMPI_v${OpenMPI_version}")
SET(CPACK_PACKAGE_FILE_NAME "OpenMPI-${OpenMPI_version}")
SET(CPACK_INSTALLED_DIRECTORIES "${CMAKE_INSTALL_PREFIX}" ".")
SET(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "Open MPI")

SET(CPACK_SOURCE_PACKAGE_FILE_NAME "OpenMPI-${VERSION_STRING}")
#SET(CPACK_SOURCE_INSTALLED_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}")
#There is a bug in NSI that does not handle full unix paths properly. Make
# sure there is at least one set of four (4) backlasshes.
# SET(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
# SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\MyExecutable.exe")
SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} OpenMPI")
SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.open-mpi.org")
SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.open-mpi.org")
# SET(CPACK_NSIS_CONTACT "me@my-personal-home-page.com")
# SET(CPACK_PACKAGE_EXECUTABLES "MyExecutable" "My Executable")


SET(CPACK_NSIS_MODIFY_PATH OFF)
SET(CPACK_SOURCE_ZIP ON)

# setup the register entries when install
SET(CPACK_NSIS_EXTRA_INSTALL_COMMANDS ${CPACK_NSIS_EXTRA_INSTALL_COMMANDS} "
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_PREFIX\\\" \\\"$INSTDIR\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_EXEC_PREFIX\\\" \\\"$INSTDIR\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_BINDIR\\\" \\\"$INSTDIR\\\\bin\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_SBINDIR\\\" \\\"$INSTDIR\\\\sbin\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_LIBEXECDIR\\\" \\\"$INSTDIR\\\\libexec\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_DATAROOTDIR\\\" \\\"$INSTDIR\\\\share\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_DATADIR\\\" \\\"$INSTDIR\\\\share\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_SYSCONFDIR\\\" \\\"$INSTDIR\\\\etc\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_LOCALSTATEDIR\\\" \\\"$INSTDIR\\\\etc\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_LIBDIR\\\" \\\"$INSTDIR\\\\lib\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_INCLUDEDIR\\\" \\\"$INSTDIR\\\\include\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_INFODIR\\\" \\\"$INSTDIR\\\\share\\\\info\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_MANDIR\\\" \\\"$INSTDIR\\\\share\\\\man\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_PKGDATADIR\\\" \\\"$INSTDIR\\\\share\\\\openmpi\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_PKGLIBDIR\\\" \\\"$INSTDIR\\\\lib\\\\openmpi\\\"
    WriteRegStr HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\" \\\"OPAL_PKGINCLUDEDIR\\\" \\\"$INSTDIR\\\\include\\\\openmpi\\\"
    ReadRegStr $0 HKLM \\\"SYSTEM\\\\CurrentControlSet\\\\Control\\\\Session Manager\\\\Environment\\\" \\\"PATH\\\"
    StrCpy $0 $0;$INSTDIR\\\\bin
    WriteRegExpandStr HKLM \\\"SYSTEM\\\\CurrentControlSet\\\\Control\\\\Session Manager\\\\Environment\\\" \\\"PATH\\\" $0
")


# delete the register entries when uninstall
SET(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS ${CPACK_NSIS_EXTRA_INSTALL_COMMANDS} "
    DeleteRegKey HKLM \\\"SOFTWARE\\\\${CPACK_PACKAGE_VENDOR}\\\"
")


# (un)install icons
#SET(CPACK_NSIS_MUI_ICON "${CMAKE_SOURCE_DIR}/contrib/platform/win32/open-mpi-logo.ico")
#SET(CPACK_NSIS_MUI_UNIICON "${CMAKE_SOURCE_DIR}/contrib/platform/win32/uninstall.ico")

INCLUDE(CPack)
