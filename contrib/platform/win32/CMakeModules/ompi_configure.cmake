#
# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
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


INCLUDE (get_c_alignment)
INCLUDE (check_c_inline)
INCLUDE (Check_c_type_exists)

INCLUDE (CheckIncludeFileCXX)
INCLUDE (CheckIncludeFile)
# usage: CHECK_INCLUDE_FILE (<header> <RESULT_VARIABLE> )
# Example: CHECK_INCLUDE_FILE(strings.h HAVE_STRINGS_H) 
# find out how to check data type/size, compiler FLAGS, 

INCLUDE (CheckFunctionExists)
# Usage: CHECK_FUNCTION_EXISTS(function variable) 
# Example: CHECK_FUNCTION_EXISTS(madvise HAVE_MADVISE)
# Checks whether the given function exists. This is done by linking a small program, which may not result in undefined references.

INCLUDE (CheckSymbolExists)
# Usage: CHECK_SYMBOL_EXISTS(symbol headers variable) 
# Example: CHECK_SYMBOL_EXISTS((LC_MESSAGES "locale.h" HAVE_LC_MESSAGES) 
# Checks whether the given symbol exists if the specified headers are included.

INCLUDE (CheckLibraryExists)
# Usage: CHECK_LIBRARY_EXISTS(library function location variable) 
# Example: CHECK_LIBRARY_EXISTS(volmgt volmgt_running "" HAVE_VOLMGT) 
# Checks whether the given library exists and contains the given function. This is done by linking a small program which uses the function and links to the library. In the location parameter an additional link directory (-Ldir) can be given if required.

INCLUDE (CheckTypeSize) 
# Usage: SET(CMAKE_EXTRA_INCLUDE_FILES header)
#        CHECK_TYPE_SIZE(type variable)
#        SET(CMAKE_EXTRA_INCLUDE_FILES) 
# Example: SET(CMAKE_EXTRA_INCLUDE_FILES sys/socket.h)
#          CHECK_TYPE_SIZE("struct ucred" STRUCT_UCRED)
#          SET(CMAKE_EXTRA_INCLUDE_FILES)
# Checks whether the specified type exists and returns the size of the type. In the variable the size of the type will be returned, additionally a variable HAVE_STRUCT_UCRED will be set to true if the type exists. Please not that you have to set CMAKE_EXTRA_INCLUDE_FILES to the required headers for this type, and you should reset it after calling CHECK_TYPE_SIZE. If you are not really interested in the size of the type, but only whether it exists or not, you can also use STRUCT_UCRED directly, if the type doesn't exist, it will be empty and so also evaluate to FALSE (as will HAVE_STRUCT_UCRED).

INCLUDE (CheckStructHasMember)
# Usage: CHECK_STRUCT_HAS_MEMBER (STRUCT MEMBER HEADER VARIABLE)
# Example: CHECK_STRUCT_HAS_MEMBER("struct timeval" tv_sec sys/select.h HAVE_TIMEVAL_TV_SEC)
# Check if the given struct or class has the specified member variable

#INCLUDE (CheckPrototypeExists) 
# Usage: CHECK_PROTOTYPE_EXISTS(function headers variable) 
# Example: CHECK_PROTOTYPE_EXISTS(mkstemps "stdlib.h;unistd.h" HAVE_MKSTEMPS_PROTO) 
# Checks whether the headers provide the declaration for the given function, i.e. it does not check whether using function will lead to undefined references.


INCLUDE (CheckCXXSourceCompiles)
INCLUDE (CheckCSourceCompiles)
# Usage: CHECK_CXX_SOURCE_COMPILES(source variable)
#        CHECK_C_SOURCE_COMPILES(source variable) 
# Checks whether the code given in source will compile and link. You can set CMAKE_REQUIRED_LIBRARIES, CMAKE_REQUIRED_FLAGS and CMAKE_REQUIRED_INCLUDES accordingly if additional libraries or compiler flags are required.

# First, let's check windows features.
IF(WIN32)
  INCLUDE(ompi_check_Microsoft)
  OMPI_MICROSOFT_COMPILER ()
ENDIF(WIN32)

# find flex command
INCLUDE (find_flex)
FIND_FLEX()

# Get current time and date.
EXECUTE_PROCESS(COMMAND cmd /C time /t
                OUTPUT_VARIABLE    CURRENT_TIME)

EXECUTE_PROCESS(COMMAND cmd /C date /t
                OUTPUT_VARIABLE    CURRENT_DATE)

STRING (REPLACE "\n" "" CURRENT_TIME ${CURRENT_TIME})
STRING (REPLACE "\n" "" CURRENT_DATE ${CURRENT_DATE})
SET (OMPI_CONFIGURE_DATE "${CURRENT_TIME} ${CURRENT_DATE}" CACHE INTERNAL "OMPI_CONFIGURE_DATE")
SET (OMPI_BUILD_DATE "${CURRENT_TIME} ${CURRENT_DATE}" CACHE INTERNAL "OMPI_BUILD_DATE")

SET(OMPI_BUILD_CFLAGS "\"/Od /Gm /EHsc /RTC1 /MDd\"")

if(${OpenMPI_SOURCE_DIR} STREQUAL ${OpenMPI_BINARY_DIR})
  SET(OMPI_BUILD_CPPFLAGS "\"-I${OpenMPI_SOURCE_DIR}/
    -I${OpenMPI_SOURCE_DIR}/opal
    -I${OpenMPI_SOURCE_DIR}/opal/include
    -I${OpenMPI_SOURCE_DIR}/ompi
    -I${OpenMPI_SOURCE_DIR}/ompi/include
    -I${OpenMPI_SOURCE_DIR}/orte
    -I${OpenMPI_SOURCE_DIR}/orte/include
    -I${OpenMPI_SOURCE_DIR}/contrib/platform/win32\"")
else(${OpenMPI_SOURCE_DIR} STREQUAL ${OpenMPI_BINARY_DIR})
  SET(OMPI_BUILD_CPPFLAGS "\"-I${OpenMPI_SOURCE_DIR}/
    -I${OpenMPI_SOURCE_DIR}/opal
    -I${OpenMPI_SOURCE_DIR}/opal/include
    -I${OpenMPI_SOURCE_DIR}/ompi
    -I${OpenMPI_SOURCE_DIR}/ompi/include
    -I${OpenMPI_SOURCE_DIR}/orte
    -I${OpenMPI_SOURCE_DIR}/orte/include
    -I${OpenMPI_BINARY_DIR}/
    -I${OpenMPI_BINARY_DIR}/opal
    -I${OpenMPI_BINARY_DIR}/opal/include
    -I${OpenMPI_BINARY_DIR}/ompi
    -I${OpenMPI_BINARY_DIR}/ompi/include
    -I${OpenMPI_BINARY_DIR}/orte
    -I${OpenMPI_BINARY_DIR}/orte/include
    -I${OpenMPI_SOURCE_DIR}/contrib/platform/win32\"")

endif(${OpenMPI_SOURCE_DIR} STREQUAL ${OpenMPI_BINARY_DIR})

SET(OMPI_BUILD_CXXFLAGS "\"/Od /Gm /EHsc /RTC1 /MDd\"")

SET(OMPI_BUILD_CXXCPPFLAGS ${OMPI_BUILD_CPPFLAGS})
  
SET(OMPI_BUILD_FFLAGS "\"\"")

SET(OMPI_BUILD_FCFLAGS "\"\"")

SET(OMPI_BUILD_LDFLAGS "\"\"")

SET(OMPI_BUILD_LIBS "\"\"")

SET(OMPI_F77_ABSOLUTE "\"none\"")

SET(OMPI_F90_ABSOLUTE "\"none\"")

SET(OMPI_F90_BUILD_SIZE "\"small\"")

# set the im/export decleration here. 
# Don't bother with OMPI_IMPORTS
IF(BUILD_SHARED_LIBS)
  SET(OMPI_DECLSPEC "__declspec(dllimport)")
ELSE(BUILD_SHARED_LIBS)
  SET(OMPI_DECLSPEC "")
ENDIF(BUILD_SHARED_LIBS)

###################################################################
#                              Options                            #
###################################################################


OPTION(MCA_mtl_DIRECT_CALL "Whether mtl should use direct calls instead of components." OFF)
MARK_AS_ADVANCED(MCA_mtl_DIRECT_CALL)
IF(MCA_mtl_DIRECT_CALL)
  SET(MCA_mtl_DIRECT_CALL 1)
ELSE(MCA_mtl_DIRECT_CALL)
  SET(MCA_mtl_DIRECT_CALL 0)
ENDIF(MCA_mtl_DIRECT_CALL)

OPTION(MCA_pml_DIRECT_CALL "Whether pml should use direct calls instead of components." OFF)
MARK_AS_ADVANCED(MCA_pml_DIRECT_CALL)
IF(MCA_pml_DIRECT_CALL)
  SET(MCA_pml_DIRECT_CALL 1)
ELSE(MCA_pml_DIRECT_CALL)
  SET(MCA_pml_DIRECT_CALL 0)
ENDIF(MCA_pml_DIRECT_CALL)

OPTION(MPI_PARAM_CHECK
  "Whether we want to check MPI parameters always, never, or decide at run-time." OFF)
MARK_AS_ADVANCED(MPI_PARAM_CHECK)
IF(NOT MPI_PARAM_CHECK)
  SET (MPI_PARAM_CHECK "ompi_mpi_param_check")
ENDIF(NOT MPI_PARAM_CHECK)

OPTION(OMPI_ENABLE_DEBUG 
  "Whether we want developer-level debugging code or not." OFF)
IF(OMPI_ENABLE_DEBUG)
  SET(OMPI_ENABLE_DEBUG 1)
ELSE(OMPI_ENABLE_DEBUG)
  SET(OMPI_ENABLE_DEBUG 0)
ENDIF(OMPI_ENABLE_DEBUG)

OPTION(OMPI_ENABLE_HETEROGENEOUS_SUPPORT
  "Enable features required for heterogeneous support." OFF)
MARK_AS_ADVANCED(OMPI_ENABLE_HETEROGENEOUS_SUPPORT)
IF(OMPI_ENABLE_HETEROGENEOUS_SUPPORT)
  SET(OMPI_ENABLE_HETEROGENEOUS_SUPPORT 1)
ELSE(OMPI_ENABLE_HETEROGENEOUS_SUPPORT)
  SET(OMPI_ENABLE_HETEROGENEOUS_SUPPORT 0)
ENDIF(OMPI_ENABLE_HETEROGENEOUS_SUPPORT)

OPTION(OMPI_ENABLE_MEM_DEBUG
  "Whether we want the memory debug or not." OFF)
MARK_AS_ADVANCED(OMPI_ENABLE_MEM_DEBUG)
IF(OMPI_ENABLE_MEM_DEBUG)
  SET(OMPI_ENABLE_MEM_DEBUG 1)
ELSE(OMPI_ENABLE_MEM_DEBUG)
  SET(OMPI_ENABLE_MEM_DEBUG 0)
ENDIF(OMPI_ENABLE_MEM_DEBUG)

OPTION(OMPI_ENABLE_MEM_PROFILE
  "Whether we want the memory profiling or not." OFF)
MARK_AS_ADVANCED(OMPI_ENABLE_MEM_PROFILE)
IF(OMPI_ENABLE_MEM_PROFILE)
  SET(OMPI_ENABLE_MEM_PROFILE 1)
ELSE(OMPI_ENABLE_MEM_PROFILE)
  SET(OMPI_ENABLE_MEM_PROFILE 0)
ENDIF(OMPI_ENABLE_MEM_PROFILE)

OPTION(OMPI_ENABLE_MPI_PROFILING
  "Whether we want MPI profiling or not." ON)
MARK_AS_ADVANCED(OMPI_ENABLE_MPI_PROFILING)
IF(OMPI_ENABLE_MPI_PROFILING)
  SET(OMPI_ENABLE_MPI_PROFILING 1)
  SET(MPIF_H_PMPI_W_FUNCS ", PMPI_WTICK, PMPI_WTIME")
ELSE(OMPI_ENABLE_MPI_PROFILING)
  SET(OMPI_ENABLE_MPI_PROFILING 0)
  SET(MPIF_H_PMPI_W_FUNCS "")
ENDIF(OMPI_ENABLE_MPI_PROFILING)

OPTION(OMPI_ENABLE_MPI_THREADS
  "Whether we should enable support for multiple user threads." OFF)
MARK_AS_ADVANCED(OMPI_ENABLE_MPI_THREADS)
IF(OMPI_ENABLE_MPI_THREADS)
  SET(OMPI_ENABLE_MPI_THREADS 1)
ELSE(OMPI_ENABLE_MPI_THREADS)
  SET(OMPI_ENABLE_MPI_THREADS 0)
ENDIF(OMPI_ENABLE_MPI_THREADS)

OPTION(OMPI_ENABLE_PROGRESS_THREADS
  "Whether we should use progress threads rather than polling." OFF)
MARK_AS_ADVANCED(OMPI_ENABLE_PROGRESS_THREADS)
IF(OMPI_ENABLE_PROGRESS_THREADS)
  SET(OMPI_ENABLE_PROGRESS_THREADS 1)
ELSE(OMPI_ENABLE_PROGRESS_THREADS)
  SET(OMPI_ENABLE_PROGRESS_THREADS 0)
ENDIF(OMPI_ENABLE_PROGRESS_THREADS)

OPTION(OMPI_ENABLE_PTY_SUPPORT
  "Whether we should use progress threads rather than polling." OFF)
MARK_AS_ADVANCED(OMPI_ENABLE_PTY_SUPPORT)
IF(OMPI_ENABLE_PTY_SUPPORT)
  SET(OMPI_ENABLE_PTY_SUPPORT 1)
ELSE(OMPI_ENABLE_PTY_SUPPORT)
  SET(OMPI_ENABLE_PTY_SUPPORT 0)
ENDIF(OMPI_ENABLE_PTY_SUPPORT)

OPTION ( OMPI_GROUP_SPARSE
  "Wether we want sparse process groups." OFF)
MARK_AS_ADVANCED(OMPI_GROUP_SPARSE)
IF(NOT OMPI_GROUP_SPARSE)
  SET (OMPI_GROUP_SPARSE 0)
ELSE(NOT OMPI_GROUP_SPARSE)
  SET (OMPI_GROUP_SPARSE 1)
ENDIF(NOT OMPI_GROUP_SPARSE)

OPTION (OMPI_PROVIDE_MPI_FILE_INTERFACE
 "Whether OMPI should provide MPI File interface" ON)
MARK_AS_ADVANCED(OMPI_PROVIDE_MPI_FILE_INTERFACE)
IF(OMPI_PROVIDE_MPI_FILE_INTERFACE)
  SET (OMPI_PROVIDE_MPI_FILE_INTERFACE 1)
ELSE(OMPI_PROVIDE_MPI_FILE_INTERFACE)
  SET (OMPI_PROVIDE_MPI_FILE_INTERFACE 0)
ENDIF(OMPI_PROVIDE_MPI_FILE_INTERFACE)


OPTION(OMPI_WANT_CXX_BINDINGS
  "Whether we want MPI cxx support or not." ON)
MARK_AS_ADVANCED(OMPI_WANT_CXX_BINDINGS)
IF(OMPI_WANT_CXX_BINDINGS)
  SET(OMPI_WANT_CXX_BINDINGS 1)
ELSE(OMPI_WANT_CXX_BINDINGS)
  SET(OMPI_WANT_CXX_BINDINGS 0)
ENDIF(OMPI_WANT_CXX_BINDINGS)

OPTION(OMPI_WANT_F77_BINDINGS
  "Whether we want MPI F77 support or not." OFF)
MARK_AS_ADVANCED(OMPI_WANT_F77_BINDINGS)
IF(OMPI_WANT_F77_BINDINGS)
  SET(OMPI_WANT_F77_BINDINGS 1)
ELSE(OMPI_WANT_F77_BINDINGS)
  SET(OMPI_WANT_F77_BINDINGS 0)
ENDIF(OMPI_WANT_F77_BINDINGS)

OPTION(OMPI_WANT_F90_BINDINGS
  "Whether we want MPI F90 support or not." OFF)
MARK_AS_ADVANCED(OMPI_WANT_F90_BINDINGS)
IF(OMPI_WANT_F90_BINDINGS)
  SET(OMPI_WANT_F90_BINDINGS 1)
ELSE(OMPI_WANT_F90_BINDINGS)
  SET(OMPI_WANT_F90_BINDINGS 0)
ENDIF(OMPI_WANT_F90_BINDINGS)

OPTION(OMPI_WANT_MPI_CXX_SEEK
  "Do we want to try to work around C++ bindings SEEK_* issue?" OFF)
MARK_AS_ADVANCED(OMPI_WANT_MPI_CXX_SEEK)

OPTION(OMPI_WANT_PERUSE
  "Whether the peruse interface should be enabled." OFF)
MARK_AS_ADVANCED(OMPI_WANT_PERUSE)
IF(NOT OMPI_WANT_PERUSE)
  SET(OMPI_WANT_PERUSE 0)
ELSE(NOT OMPI_WANT_PERUSE)
  SET(OMPI_WANT_PERUSE 1)
ENDIF(NOT OMPI_WANT_PERUSE)

OPTION( OMPI_WANT_PRETTY_PRINT_STACKTRACE
  "Whether we want pretty-print stack trace feature." ON)
MARK_AS_ADVANCED(OMPI_WANT_PRETTY_PRINT_STACKTRACE)

OPTION( OMPI_WANT_SMP_LOCKS
  "Whether we want to have smp locks in atomic ops or not." ON)
MARK_AS_ADVANCED(OMPI_WANT_SMP_LOCKS)

OPTION( OPAL_ENABLE_FT 
  "Enable fault tolerance general components and logic." ON)
MARK_AS_ADVANCED(OPAL_ENABLE_FT)
IF(NOT OPAL_ENABLE_FT)
  SET(OPAL_ENABLE_FT 0)
ELSE(NOT OPAL_ENABLE_FT)
  SET(OPAL_ENABLE_FT 1)
ENDIF(NOT OPAL_ENABLE_FT)

OPTION( OPAL_ENABLE_FT_CR 
  "Enable fault tolerance checkpoint/restart components and logic." OFF)
MARK_AS_ADVANCED(OPAL_ENABLE_FT_CR)

OPTION( OPAL_ENABLE_FT_THREAD 
  "Enable fault tolerance thread in Open PAL." OFF)
MARK_AS_ADVANCED(OPAL_ENABLE_FT_THREAD)
IF(NOT OPAL_ENABLE_FT_THREAD)
  SET(OPAL_ENABLE_FT_THREAD 0)
ELSE(NOT OPAL_ENABLE_FT_THREAD)
  SET(OPAL_ENABLE_FT_THREAD 1)
ENDIF(NOT OPAL_ENABLE_FT_THREAD)

OPTION( OPAL_ENABLE_IPV6 
  "Enable IPv6 support, but only if the underlying system supports it." ON)
MARK_AS_ADVANCED(OPAL_ENABLE_IPV6)

OPTION( OPAL_ENABLE_TRACE 
  "Enable run-time tracing of internal functions." OFF)
MARK_AS_ADVANCED(OPAL_ENABLE_TRACE)

OPTION(ORTE_DISABLE_FULL_SUPPORT "Enable full RTE support (Default OFF)." OFF)
MARK_AS_ADVANCED(ORTE_DISABLE_FULL_SUPPORT)
IF(NOT ORTE_DISABLE_FULL_SUPPORT)
  SET(ORTE_DISABLE_FULL_SUPPORT 0)
ELSE(NOT ORTE_DISABLE_FULL_SUPPORT)
  SET (ORTE_DISABLE_FULL_SUPPORT 1)
ENDIF(NOT ORTE_DISABLE_FULL_SUPPORT)

OPTION( ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT 
  "Whether we want orterun to effect \"--prefix $prefix\" by default." ON)
MARK_AS_ADVANCED(ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT)
IF(NOT ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT)
  SET (ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT 0)
ELSE(NOT ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT)
  SET (ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT 1)
ENDIF(NOT ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT)

OPTION(OMPI_WANT_LIBLTDL "Whether we want to enable DSO build for Windows." OFF) 
IF(NOT OMPI_WANT_LIBLTDL) 
  SET(OMPI_WANT_LIBLTDL 0) 
ELSE(NOT OMPI_WANT_LIBLTDL) 
  INCLUDE(find_libltdl) 
  IF(LIBLTDL_FOUND) 
    SET (OMPI_WANT_LIBLTDL 1) 
  ELSE(LIBLTDL_FOUND) 
    SET(OMPI_WANT_LIBLTDL 0) 
  ENDIF(LIBLTDL_FOUND) 
ENDIF(NOT OMPI_WANT_LIBLTDL) 


###################################################################
#                           Check headers                         #
###################################################################

#/* Define to 1 if you have the <aio.h> header file. */
CHECK_INCLUDE_FILE (aio.h HAVE_AIO_H)

#/* Define to 1 if you have the <alloca.h> header file. */
CHECK_INCLUDE_FILE (alloca.h HAVE_ALLOCA_H)

#/* Define to 1 if you have the <arpa/inet.h> header file. */
CHECK_INCLUDE_FILE (arpa/inet.h HAVE_ARPA_INET_H)

#/* Define to 1 if you have the <crt_externs.h> header file. */
CHECK_INCLUDE_FILE (crt_externs.h HAVE_CRT_EXTERNS_H)

#/* Define to 1 if you have the <dirent.h> header file. */
CHECK_INCLUDE_FILE (dirent.h HAVE_DIRENT_H)

#/* Define to 1 if you have the <dlfcn.h> header file. */
CHECK_INCLUDE_FILE (dlfcn.h HAVE_DLFCN_H)

#/* Define to 1 if you have the <err.h> header file. */
CHECK_INCLUDE_FILE (err.h HAVE_ERR_H)

#/* Define to 1 if you have the <execinfo.h> header file. */
CHECK_INCLUDE_FILE (execinfo.h HAVE_EXECINFO_H)

#/* Define to 1 if you have the <fcntl.h> header file. */
CHECK_INCLUDE_FILE (fcntl.h HAVE_FCNTL_H)

#/* Define to 1 if you have the <grp.h> header file. */
CHECK_INCLUDE_FILE (grp.h HAVE_GRP_H)

#/* Define to 1 if you have the <ifaddrs.h> header file. */
CHECK_INCLUDE_FILE (ifaddrs.h HAVE_IFADDRS_H)

#/* Define to 1 if you have the <inttypes.h> header file. */
CHECK_INCLUDE_FILE (inttypes.h HAVE_INTTYPES_H)

#/* Define to 1 if you have the <libcr.h> header file. */
CHECK_INCLUDE_FILE (libcr.h HAVE_LIBCR_H)

#/* Define to 1 if you have the <libgen.h> header file. */
CHECK_INCLUDE_FILE (libgen.h HAVE_LIBGEN_H)

#/* Define to 1 if you have the <libutil.h> header file. */
CHECK_INCLUDE_FILE (libutil.h HAVE_LIBUTIL_H)

#/* Define to 1 if you have the <libxcpu.h> header file. */
CHECK_INCLUDE_FILE (libxcpu.h HAVE_LIBXCPU_H)

#/* Define to 1 if you have the <mach/mach_time.h> header file. */
CHECK_INCLUDE_FILE (mach/mach_time.h HAVE_MACH_MACH_TIME_H)

#/* Define to 1 if you have the <mach/mach_vm.h> header file. */
CHECK_INCLUDE_FILE (mach/mach_vm.h HAVE_MACH_MACH_VM_H)

#/* Define to 1 if you have the <malloc.h> header file. */
CHECK_INCLUDE_FILE (malloc.h HAVE_MALLOC_H)

#/* Define to 1 if you have the <memory.h> header file. */
CHECK_INCLUDE_FILE (memory.h HAVE_MEMORY_H)

#/* Define to 1 if you have the <netdb.h> header file. */
CHECK_INCLUDE_FILE (netdb.h HAVE_NETDB_H)

#/* Define to 1 if you have the <netinet/in.h> header file. */
CHECK_INCLUDE_FILE (netinet/in.h HAVE_NETINET_IN_H)

#/* Define to 1 if you have the <netinet/tcp.h> header file. */
CHECK_INCLUDE_FILE (netinet/tcp.h HAVE_NETINET_TCP_H)

#/* Define to 1 if you have the <net/if.h> header file. */
CHECK_INCLUDE_FILE (net/if.h HAVE_NET_IF_H)

#/* Define to 1 if you have the <pmapi.h> header file. */
CHECK_INCLUDE_FILE (pmapi.h HAVE_PMAPI_H)

#/* Define to 1 if you have the <poll.h> header file. */
CHECK_INCLUDE_FILE (poll.h HAVE_POLL_H)

#/* Define to 1 if you have the <pthread.h> header file. */
CHECK_INCLUDE_FILE (pthread.h HAVE_PTHREAD_H)

#/* Define to 1 if you have the <pty.h> header file. */
CHECK_INCLUDE_FILE (pty.h HAVE_PTY_H)

#/* Define to 1 if you have the <pwd.h> header file. */
CHECK_INCLUDE_FILE (pwd.h HAVE_PWD_H)

#/* Define to 1 if you have the <regex.h> header file. */
CHECK_INCLUDE_FILE (regex.h HAVE_REGEX_H)

#/* Define to 1 if you have the <sched.h> header file. */
CHECK_INCLUDE_FILE (sched.h HAVE_SCHED_H)

#/* Define to 1 if you have the <signal.h> header file. */
CHECK_INCLUDE_FILE (signal.h HAVE_SIGNAL_H)

#/* Define to 1 if you have the <stdarg.h> header file. */
CHECK_INCLUDE_FILE (stdarg.h HAVE_STDARG_H)

#/* Define to 1 if you have the <stdbool.h> header file. */
CHECK_INCLUDE_FILE (stdbool.h HAVE_STDBOOL_H)

#/* Define to 1 if you have the <stdint.h> header file. */
CHECK_INCLUDE_FILE (stdint.h HAVE_STDINT_H)

#/* Define to 1 if you have the <stdlib.h> header file. */
CHECK_INCLUDE_FILE (stdlib.h HAVE_STDLIB_H)

#/* Define to 1 if you have the <strings.h> header file. */
CHECK_INCLUDE_FILE (strings.h HAVE_STRINGS_H)

#/* Define to 1 if you have the <string.h> header file. */
CHECK_INCLUDE_FILE (string.h HAVE_STRING_H)

#/* Define to 1 if you have the <stropts.h> header file. */
CHECK_INCLUDE_FILE (stropts.h HAVE_STROPTS_H)

#/* Define to 1 if you have the <syslog.h> header file. */
CHECK_INCLUDE_FILE (syslog.h HAVE_SYSLOG_H)

#/* Define to 1 if you have the <sys/bproc_common.h> header file. */
CHECK_INCLUDE_FILE (sys/bproc_common.h HAVE_SYS_BPROC_COMMON_H)

#/* Define to 1 if you have the <sys/bproc.h> header file. */
CHECK_INCLUDE_FILE (sys/bproc.h HAVE_SYS_BPROC_H)

#/* Define to 1 if you have the <sys/devpoll.h> header file. */
CHECK_INCLUDE_FILE (sys/devpoll.h HAVE_SYS_DEVPOLL_H)

#/* Define to 1 if you have the <sys/epoll.h> header file. */
CHECK_INCLUDE_FILE (sys/epoll.h HAVE_SYS_EPOLL_H)

#/* Define to 1 if you have the <sys/event.h> header file. */
CHECK_INCLUDE_FILE (sys/event.h HAVE_SYS_EVENT_H)

#/* Define to 1 if you have the <sys/fcntl.h> header file. */
CHECK_INCLUDE_FILE (sys/fcntl.h HAVE_SYS_FCNTL_H)

#/* Define to 1 if you have the <sys/ioctl.h> header file. */
CHECK_INCLUDE_FILE (sys/ioctl.h HAVE_SYS_IOCTL_H)

#/* Define to 1 if you have the <sys/ipc.h> header file. */
CHECK_INCLUDE_FILE (sys/ipc.h HAVE_SYS_IPC_H)

#/* Define to 1 if you have the <sys/mman.h> header file. */
CHECK_INCLUDE_FILE (sys/mman.h HAVE_SYS_MMAN_H)

#/* Define to 1 if you have the <sys/param.h> header file. */
CHECK_INCLUDE_FILE (sys/param.h HAVE_SYS_PARAM_H)

#/* Define to 1 if you have the <sys/queue.h> header file. */
CHECK_INCLUDE_FILE (sys/queue.h HAVE_SYS_QUEUE_H)

#/* Define to 1 if you have the <sys/resource.h> header file. */
CHECK_INCLUDE_FILE (sys/resource.h HAVE_SYS_RESOURCE_H)

#/* Define to 1 if you have the <sys/select.h> header file. */
CHECK_INCLUDE_FILE (sys/select.h HAVE_SYS_SELECT_H)

#/* Define to 1 if you have the <sys/socket.h> header file. */
CHECK_INCLUDE_FILE (sys/socket.h HAVE_SYS_SOCKET_H)

#/* Define to 1 if you have the <sys/sockio.h> header file. */
CHECK_INCLUDE_FILE (sys/sockio.h HAVE_SYS_SOCKIO_H)

#/* Define to 1 if you have the <sys/statvfs.h> header file. */
CHECK_INCLUDE_FILE (sys/statvfs.h HAVE_SYS_STATVFS_H)

#/* Define to 1 if you have the <sys/stat.h> header file. */
CHECK_INCLUDE_FILE (sys/stat.h HAVE_SYS_STAT_H)

#/* Define to 1 if you have the <sys/sysctl.h> header file. */
CHECK_INCLUDE_FILE (sys/sysctl.h HAVE_SYS_SYSCTL_H)

#/* Define to 1 if you have the <sys/time.h> header file. */
CHECK_INCLUDE_FILE (sys/time.h HAVE_SYS_TIME_H)

#/* Define to 1 if you have the <sys/tree.h> header file. */
CHECK_INCLUDE_FILE (sys/tree.h HAVE_SYS_TREE_H)

#/* Define to 1 if you have the <sys/types.h> header file. */
CHECK_INCLUDE_FILE (sys/types.h HAVE_SYS_TYPES_H)

#/* Define to 1 if you have the <sys/uio.h> header file. */
CHECK_INCLUDE_FILE (sys/uio.h HAVE_SYS_UIO_H)

#/* Define to 1 if you have the <sys/utsname.h> header file. */
CHECK_INCLUDE_FILE (sys/utsname.h HAVE_SYS_UTSNAME_H)

#/* Define to 1 if you have the <sys/wait.h> header file. */
CHECK_INCLUDE_FILE (sys/wait.h HAVE_SYS_WAIT_H)

#/* Define to 1 if you have the <termios.h> header file. */
CHECK_INCLUDE_FILE (termios.h HAVE_TERMIOS_H)

#/* Define if timeradd is defined in <sys/time.h> */
CHECK_INCLUDE_FILE (sys/time.h HAVE_TIMERADD)

#/* Define to 1 if you have the <time.h> header file. */
CHECK_INCLUDE_FILE (time.h HAVE_TIME_H)

#/* Define to 1 if you have the <ucontext.h> header file. */
CHECK_INCLUDE_FILE (ucontext.h HAVE_UCONTEXT_H)

#/* Define to 1 if you have the <ulimit.h> header file. */
CHECK_INCLUDE_FILE (ulimit.h HAVE_ULIMIT_H)

#/* Define to 1 if you have the <unistd.h> header file. */
CHECK_INCLUDE_FILE (unistd.h HAVE_UNISTD_H)

#/* Define to 1 if you have the <util.h> header file. */
CHECK_INCLUDE_FILE (util.h HAVE_UTIL_H)

#/* Define to 1 if you have the <utmp.h> header file. */
CHECK_INCLUDE_FILE (utmp.h HAVE_UTMP_H)

#/* The MX library have support for the mx_extensions.h */
CHECK_INCLUDE_FILE (mx_extension.h MX_HAVE_EXTENSIONS_H)

#/* Define to 1 if you have the ANSI C header files. */
CHECK_INCLUDE_FILE(stddef.h STDC_HEADERS)
IF(STDC_HEADERS)
SET(OMPI_STDC_HEADERS 1)
ENDIF(STDC_HEADERS)

###################################################################
#                          Check functions                        #
###################################################################

#/* Define to 1 if you have the `asprintf' function. */
CHECK_FUNCTION_EXISTS (asprintf HAVE_ASPRINTF)

#/* Define to 1 if you have the `backtrace' function. */
CHECK_FUNCTION_EXISTS (backtrace HAVE_BACKTRACE)

#/* Define to 1 if you have the `ceil' function. */
CHECK_FUNCTION_EXISTS (ceil HAVE_CEIL)

#/* Define to 1 if you have the `cnos_pm_barrier' function. */
CHECK_FUNCTION_EXISTS (cnos_pm_barrier HAVE_CNOS_PM_BARRIER)

#/* Define to 1 if you have the `dirname' function. */
CHECK_FUNCTION_EXISTS (dirname HAVE_DIRNAME)

#/* Define to 1 if you have the `dlsym' function. */
CHECK_FUNCTION_EXISTS (dlsym AVE_DLSYM)

#/* Define if your system supports the epoll system calls */
CHECK_FUNCTION_EXISTS (epoll HAVE_EPOLL)

#/* Define to 1 if you have the `epoll_ctl' function. */
CHECK_FUNCTION_EXISTS (epoll_ctl HAVE_EPOLL_CTL)

#/* Define to 1 if you have the `execve' function. */
CHECK_FUNCTION_EXISTS (execve HAVE_EXECVE)

#/* Define to 1 if you have the `fcntl' function. */
CHECK_FUNCTION_EXISTS (fcntl HAVE_FCNTL)

#/* Define to 1 if you have the `fork' function. */
CHECK_FUNCTION_EXISTS (fork HAVE_FORK)

#/* Define to 1 if you have the `getpwuid' function. */
CHECK_FUNCTION_EXISTS (getpwuid HAVE_GETPWUID)

#/* Define to 1 if you have the `gettimeofday' function. */
CHECK_FUNCTION_EXISTS (gettimeofday HAVE_GETTIMEOFDAY)

#/* Define to 1 if you have the `htonl' function. */
CHECK_FUNCTION_EXISTS (htonl HAVE_HTONL)

#/* Define to 1 if you have the `htons' function. */
CHECK_FUNCTION_EXISTS (htons HAVE_HTONS)

#/* Define to 1 if you have the `ibv_fork_init' function. */
CHECK_FUNCTION_EXISTS (ibv_fork_init HAVE_IBV_FORK_INIT)

#/* Define to 1 if you have the `ibv_get_device_list' function. */
CHECK_FUNCTION_EXISTS (ibv_get_device_list HAVE_IBV_GET_DEVICE_LIST)

#/* Define to 1 if you have the `ibv_resize_cq' function. */
CHECK_FUNCTION_EXISTS (ibv_resize_cq HAVE_IBV_RESIZE_CQ)

#/* Define to 1 if you have the `isatty' function. */
CHECK_FUNCTION_EXISTS (isatty HAVE_ISATTY)

#/* Define to 1 if you have the `killrank' function. */
CHECK_FUNCTION_EXISTS (killrank HAVE_KILLRANK)

#/* Define to 1 if you have the `kqueue' function. */
CHECK_FUNCTION_EXISTS (kqueue KQUEUE)

#/* Define to 1 if you have the `mach_vm_read' function. */
CHECK_FUNCTION_EXISTS (mach_vm_read HAVE_MACH_MACH_VM_READ)

#/* Define to 1 if you have the `mach_vm_region' function. */
CHECK_FUNCTION_EXISTS (mach_vm_region HAVE_MACH_VM_REGION)

#/* Define to 1 if you have the `mallopt' function. */
CHECK_FUNCTION_EXISTS (mallopt HAVE_MALLOPT)

#/* Define to 1 if you have the `mmap' function. */
CHECK_FUNCTION_EXISTS (mmap HAVE_MMAP)

#/* Define to 1 if you have the `ntohl' function. */
CHECK_FUNCTION_EXISTS (ntohl HAVE_NTOHL)

#/* Define to 1 if you have the `ntohs' function. */
CHECK_FUNCTION_EXISTS (ntohs HAVE_NTOHS)

#/* Define to 1 if you have the `openpty' function. */
CHECK_FUNCTION_EXISTS (openpty HAVE_OPENPTY)

#/* Define to 1 if you have the `pipe' function. */
CHECK_FUNCTION_EXISTS (pipe HAVE_PIPE)

#/* Define to 1 if you have the `pm_cycles' function. */
CHECK_FUNCTION_EXISTS (pm_cycles HAVE_PM_CYCLES)

#/* Define to 1 if you have the `poll' function. */
CHECK_FUNCTION_EXISTS (poll HAVE_POLL)

#/* Define to 1 if you have the `posix_memalign' function. */
CHECK_FUNCTION_EXISTS (posix_memalign HAVE_POSIX_MEMALIGN)

#/* Define to 1 if you have the `printstack' function. */
CHECK_FUNCTION_EXISTS (printstack HAVE_PRINTSTACK)

#/* Define to 1 if you have the `ptsname' function. */
CHECK_FUNCTION_EXISTS (ptsname HAVE_PTSNAME)

#/* Define to 1 if you have the `regcmp' function. */
CHECK_FUNCTION_EXISTS (regcmp HAVE_REGCMP)

#/* Define to 1 if you have the `regexec' function. */
CHECK_FUNCTION_EXISTS (regexec HAVE_REGEXEC)

#/* Define to 1 if you have the `regfree' function. */
CHECK_FUNCTION_EXISTS (regfree HAVE_REGFREE)

#/* Define to 1 if you have the `sched_yield' function. */
CHECK_FUNCTION_EXISTS (sched_yield HAVE_SCHED_YIELD)

#/* Define to 1 if you have the `select' function. */
CHECK_FUNCTION_EXISTS (select HAVE_SELECT)

#/* Define to 1 if you have the `setsid' function. */
CHECK_FUNCTION_EXISTS (setsid HAVE_SETSID)

#/* Define to 1 if you have the `sigtimedwait' function. */
CHECK_FUNCTION_EXISTS (sigtimedwait HAVE_SIGTIMEDWAIT)

#/* Define to 1 if you have the `snprintf' function. */
CHECK_FUNCTION_EXISTS (snprintf HAVE_SNPRINTF)

#/* Define to 1 if you have the `strsignal' function. */
CHECK_FUNCTION_EXISTS (strsignal HAVE_STRSIGNAL)

#/* Define to 1 if you have the `syscall' function. */
CHECK_FUNCTION_EXISTS (syscall HAVE_SYSCALL)

#/* Define to 1 if you have the `sysconf' function. */
CHECK_FUNCTION_EXISTS (sysconf HAVE_SYSCONF)

#/* Define to 1 if you have the `syslog' function. */
CHECK_FUNCTION_EXISTS (syslog HAVE_SYSLOG)

#/* Define to 1 if you have the `tcgetpgrp' function. */
CHECK_FUNCTION_EXISTS (tcgetpgrp HAVE_TCGETPGRP)

#/* Define to 1 if you have the `vasprintf' function. */
CHECK_FUNCTION_EXISTS (vasprintf HAVE_VASPRINTF)

#/* Define to 1 if you have the `vm_read_overwrite' function. */
CHECK_FUNCTION_EXISTS (vm_read_overwrite HAVE_VM_READ_OVERWRITE)

#/* Define to 1 if you have the `vsnprintf' function. */
CHECK_FUNCTION_EXISTS (vsnprintf HAVE_VSNPRINTF)

#/* Define to 1 if you have the `waitpid' function. */
CHECK_FUNCTION_EXISTS (waitpid HAVE_WAITPIN)

#/* Define to 1 if you have the `_NSGetEnviron' function. */
CHECK_FUNCTION_EXISTS (_NSGetEnviron HAVE__NSGETENVIRON)

#/* Define to 1 if you have the `__mmap' function. */
CHECK_FUNCTION_EXISTS (__mmap HAVE___MMAP)

#/* Define to 1 if you have the `__munmap' function. */
CHECK_FUNCTION_EXISTS (__munmap HAVE___MUNMAP)

###################################################################
#                          Check libraries                        #
###################################################################


###################################################################
#                           Check symbols                         #
###################################################################

#/* Define if F_SETFD is defined in <fcntl.h> */
CHECK_SYMBOL_EXISTS (F_SETFD fcntl.h HAVE_SETFD)

#/* Define if TAILQ_FOREACH is defined in <sys/queue.h> */
CHECK_SYMBOL_EXISTS (TAILQ_FOREACH "sys/queue.h" HAVE_TAILQFOREACH) 

#/* Define to 1 if you have the declaration of `IBV_EVENT_CLIENT_REREGISTER',
#   and to 0 if you don't. */
CHECK_SYMBOL_EXISTS (IBV_EVENT_CLIENT_REREGISTER "" HAVE_DECL_IBV_EVENT_CLIENT_REREGISTER)

#/* Define to 1 if you have the declaration of `AF_UNSPEC', and to 0 if you
#   don't. */
CHECK_SYMBOL_EXISTS (AF_UNSPEC winsock2.h HAVE_DECL_AF_UNSPEC)

#/* Define to 1 if you have the declaration of `PF_UNSPEC', and to 0 if you
#   don't. */
CHECK_SYMBOL_EXISTS (PF_UNSPEC winsock2.h HAVE_DECL_PF_UNSPEC)

#/* Define to 1 if you have the declaration of `AF_INET6', and to 0 if you
#   don't. */
CHECK_SYMBOL_EXISTS (AF_INET6 winsock2.h HAVE_DECL_AF_INET6)

#/* Define to 1 if you have the declaration of `PF_INET6', and to 0 if you
#   don't. */
CHECK_SYMBOL_EXISTS (PF_INET6 winsock2.h HAVE_DECL_PF_INET6)

#
#/* Define to 1 if you have the declaration of `RLIMIT_NPROC', and to 0 if you
#   don't. */
CHECK_SYMBOL_EXISTS (RLIMIT_NPROC "" HAVE_DECL_RLIMIT_NPROC)

#/* Define to 1 if you have the declaration of `sbrk', and to 0 if you don't.
#   */
CHECK_SYMBOL_EXISTS (sbrk "" HAVE_DECL_SBRK)

#/* Define to 1 if you have the declaration of `__func__', and to 0 if you
#   don't. */
CHECK_SYMBOL_EXISTS (__func__ "" HAVE_DECL___FUNC__)
IF(NOT HAVE_DECL___FUNC__)
  SET(HAVE_DECL___FUNC__ 0)
ENDIF(NOT HAVE_DECL___FUNC__)

#/* Define to 1 if `srr0' is member of `ppc_thread_state_t'. */
CHECK_STRUCT_HAS_MEMBER(ppc_thread_state_t srr0 mach/ppc/thread_status.h HAVE_PPC_THREAD_STATE_T_SRR0)

#/* Define to 1 if `si_band' is member of `siginfo_t'. */
CHECK_STRUCT_HAS_MEMBER(siginfo_t si_band sys/siginfo.h HAVE_SIGINFO_T_SI_BAND)

#/* Define to 1 if `si_fd' is member of `siginfo_t'. */
CHECK_STRUCT_HAS_MEMBER(siginfo_t si_fd sys/siginfo.h HAVE_SIGINFO_T_SI_FD)

#/* Define to 1 if `d_type' is member of `struct dirent'. */
CHECK_STRUCT_HAS_MEMBER("struct dirent" d_type dirent.h HAVE_STRUCT_DIRENT_D_TYPE)


###################################################################
#                         Check data type                         #
###################################################################

#/* The size of `bool', as computed by sizeof. */
INCLUDE(check_sizeof_bool)
CHECK_SIZEOF_BOOL()

#/* The size of `char', as computed by sizeof. */
CHECK_TYPE_SIZE(char CHAR)
SET(SIZEOF_CHAR ${CHAR} CACHE INTERNAL "sizeof 'char'")

#/* The size of `double', as computed by sizeof. */
CHECK_TYPE_SIZE(double DOUBLE)
SET(SIZEOF_DOUBLE ${DOUBLE} CACHE INTERNAL "sizeof 'double'")

#/* The size of `float', as computed by sizeof. */
CHECK_TYPE_SIZE(float FLOAT)
SET(SIZEOF_FLOAT ${FLOAT} CACHE INTERNAL "sizeof 'float'")

#/* The size of `int', as computed by sizeof. */
CHECK_TYPE_SIZE(int INT)
SET(SIZEOF_INT ${INT} CACHE INTERNAL "sizeof int")

#/* The size of `long', as computed by sizeof. */
CHECK_TYPE_SIZE(long LONG)
SET(SIZEOF_LONG ${LONG} CACHE INTERNAL "sizeof 'long'")

#/* The size of `long double', as computed by sizeof. */
CHECK_TYPE_SIZE("long double" LONG_DOUBLE)
SET(SIZEOF_LONG_DOUBLE ${LONG_DOUBLE} CACHE INTERNAL "sizeof 'long double'")

#/* The size of `long long', as computed by sizeof. */
CHECK_TYPE_SIZE("long long" LONG_LONG)
SET(SIZEOF_LONG_LONG ${LONG_LONG} CACHE INTERNAL "sizeof 'long long'")

#/* The size of `unsigned int', as computed by sizeof. */
CHECK_TYPE_SIZE ("unsigned int" UNSIGNED_INT)
SET(SIZEOF_UNSIGNED_INT ${UNSIGNED_INT} CACHE INTERNAL "sizeof 'unsigned int'")

#/* The size of `unsigned short', as computed by sizeof. */
CHECK_TYPE_SIZE ("unsigned short" UNSIGNED_SHORT)
SET(SIZEOF_UNSIGNED_SHORT ${UNSIGNED_SHORT} CACHE INTERNAL "sizeof 'unsigned short'")

#/* The size of `unsigned long long', as computed by sizeof. */
CHECK_TYPE_SIZE ("unsigned long long" UNSIGNED_LONG_LONG)
SET(SIZEOF_UNSIGNED_LONG_LONG ${UNSIGNED_LONG_LONG} CACHE INTERNAL "sizeof 'unsigned long long'")

#/* The size of `unsigned char', as computed by sizeof. */
CHECK_TYPE_SIZE ("unsigned char" UNSIGNED_CHAR)
SET(SIZEOF_UNSIGNED_CHAR ${UNSIGNED_CHAR} CACHE INTERNAL "sizeof 'unsigned char'")

#/* The size of `short', as computed by sizeof. */
CHECK_TYPE_SIZE(short SHORT)
SET(SIZEOF_SHORT ${SHORT} CACHE INTERNAL "sizeof 'short'")

#/* The size of `size_t', as computed by sizeof. */
CHECK_TYPE_SIZE(size_t SIZE_T)
IF (NOT SIZEOF_SIZE_T)
  # size_t is not defind, define it as unsigned int.
  MESSAGE(STATUS "Define it as 'unsigned int'.")
  SET (SIZE_T "unsigned int")
  SET (SIZEOF_SIZE_T ${SIZEOF_UNSIGNED_INT} CACHE INTERNAL "sizeof 'size_t'")
ELSEIF (SIZE_T)
  SET(SIZEOF_SIZE_T ${SIZE_T} CACHE INTERNAL "sizeof 'size_t'")
ENDIF (NOT SIZEOF_SIZE_T)

#/* The size of `ssize_t', as computed by sizeof. */
CHECK_TYPE_SIZE(ssize_t SSIZE_T)
SET(SIZEOF_SSIZE_T ${SSIZE_T} CACHE INTERNAL "sizeof sszie_t")

#/* The size of `void *', as computed by sizeof. */
CHECK_TYPE_SIZE("void *" VOID_P)
SET(SIZEOF_VOID_P ${VOID_P} CACHE INTERNAL "sizeof 'void *'")

#/* The size of `pid_t', as computed by sizeof. */
CHECK_TYPE_SIZE(pid_t PID_T)
IF (NOT SIZEOF_PID_T)
  MESSAGE(STATUS "Define it as int.")
  SET (PID_T "int" CACHE INTERNAL "define 'pid_t'")
  SET (SIZEOF_PID_T ${SIZEOF_INT} CACHE INTERNAL "sizeof 'pid_t'")
ELSEIF (HAVE_PID_T)
  SET(SIZEOF_PID_T ${PID_T} CACHE INTERNAL "sizeof 'pid_t'")
ENDIF (NOT SIZEOF_PID_T)

#/* The size of `ptrdiff_t', as computed by sizeof. */
CHECK_TYPE_SIZE(ptrdiff_t PTRDIFF_T)
SET(SIZEOF_PTRDIFF_T ${PTRDIFF_T} CACHE INTERNAL "sizeof 'ptrdiff_t'")

#/* Define to 1 if the system has the type `mode_t'. */
CHECK_TYPE_SIZE (mode_t MODE_T)

#/* Define to 1 if the system has the type `int16_t'. */
CHECK_TYPE_SIZE (int16_t INT16_T)
IF (NOT SIZEOF_INT16_T)
  # int16_t is not defind, define it as short.
  MESSAGE(STATUS "Define it as 'short'.")
  #SET (int16_t "short" CACHE INTERNAL "define 'int16_t'")
  SET (SIZEOF_INT16_T ${SIZEOF_SHORT} CACHE INTERNAL "sizeof 'int16_t'")
ELSEIF (HAVE_INT16_T)
  SET (SIZEOF_INT16_T ${INT16_T} CACHE INTERNAL "sizeof 'int16_t'")
ENDIF (NOT SIZEOF_INT16_T)

#/* Define to 1 if the system has the type `int32_t'. */
CHECK_TYPE_SIZE (int32_t INT32_T)
IF (NOT SIZEOF_INT32_T)
  MESSAGE(STATUS "Define it as 'int'.")
  #SET (int32_t "int" CACHE INTERNAL "define 'int32_t'")
  SET (SIZEOF_INT32_T ${SIZEOF_INT} CACHE INTERNAL "sizeof 'int32_t'")
ELSEIF (HAVE_INT32_T)
  SET (SIZEOF_INT32_T ${INT32_T} CACHE INTERNAL "sizeof 'int32_t'")
ENDIF (NOT SIZEOF_INT32_T)

#/* Define to 1 if the system has the type `int64_t'. */
CHECK_TYPE_SIZE (int64_t INT64_T)
IF (NOT SIZEOF_INT64_T)
  MESSAGE(STATUS "Define it as 'long long'.")
  #SET (int64_t "long long" CACHE INTERNAL "define 'int64_t'")
  SET (SIZEOF_INT64_T ${SIZEOF_LONG_LONG} CACHE INTERNAL "sizeof 'int64_t'")
ELSEIF (HAVE_INT64_T)
  SET (SIZEOF_INT64_T ${INT64_T} CACHE INTERNAL "sizeof 'int64_t'")
ENDIF (NOT SIZEOF_INT64_T)

#/* Define to 1 if the system has the type `int8_t'. */
CHECK_TYPE_SIZE (int8_t INT8_T)
IF (NOT SIZEOF_INT8_T)
  MESSAGE(STATUS "Define it as 'char'.")
  #SET (int8_t "char" CACHE INTERNAL "define 'int8_t'")
  SET (SIZEOF_INT8_T ${SIZEOF_UNSIGNED_CHAR} CACHE INTERNAL "sizeof 'int8_t'")
ELSEIF (HAVE_INT8_T)
  SET (SIZEOF_INT8_T ${INT8_T} CACHE INTERNAL "sizeof 'int8_t'")
ENDIF (NOT SIZEOF_INT8_T)

#/* Define to 1 if the system has the type `uint16_t'. */
CHECK_TYPE_SIZE (uint16_t UINT16_T)
IF (NOT SIZEOF_UINT16_T)
  MESSAGE(STATUS "Define it as 'unsigned short'.")
  #SET (u_int16_t "unsigned short" CACHE INTERNAL "define 'uint16_t'")
  SET (SIZEOF_UINT16_T ${UNSIGNED_SHORT} CACHE INTERNAL "sizeof 'uint16_t'")
ELSEIF (HAVE_UINT16_T)
  SET (SIZEOF_UINT16_T ${UINT16_T} CACHE INTERNAL "sizeof 'uint16_t'")
ENDIF (NOT SIZEOF_UINT16_T)

#/* Define to 1 if the system has the type `uint32_t'. */
CHECK_TYPE_SIZE (uint32_t UNIT32_T)
IF (NOT SIZEOF_UINT32_T)
  MESSAGE(STATUS "Define it as 'unsigned int'.")
  #SET (u_int32_t "unsigned int" CACHE INTERNAL "define 'uint32_t'")
  SET (SIZEOF_UINT32_T ${SIZEOF_UNSIGNED_INT} CACHE INTERNAL "sizeof 'uint32_t'")
ELSEIF (HAVE_UINT32_T)
  SET (SIZEOF_UINT32_T ${UNIT32_T} CACHE INTERNAL "sizeof 'uint32_t'")
ENDIF (NOT SIZEOF_UINT32_T)

#/* Define to 1 if the system has the type `uint64_t'. */
CHECK_TYPE_SIZE (unit64_t UNIT64_T)
IF (NOT SIZEOF_UINT64_T)
  MESSAGE(STATUS "Define it as 'unsigned long long'.")
  #SET (u_int64_t "unsigned long long" CACHE INTERNAL "define 'uint64_t'")
  SET (SIZEOF_UINT64_T ${SIZEOF_UNSIGNED_LONG_LONG} CACHE INTERNAL "sizeof 'uint64_t'")
ELSEIF (HAVE_UINT64_T)
  SET (SIZEOF_UINT64_T ${UNIT64_T} CACHE INTERNAL "sizeof 'uint64_t'")
ENDIF (NOT SIZEOF_UINT64_T)

#/* Define to 1 if the system has the type `uint8_t'. */
CHECK_TYPE_SIZE (unit8_t UNIT8_T)
IF (NOT SIZEOF_UINT8_T)
  MESSAGE(STATUS "Define it as 'unsigned char'.")
  #SET (u_int8_t "unsigned char" CACHE INTERNAL "define 'uint8_t'")
  SET (SIZEOF_UINT8_T ${SIZEOF_UNSIGNED_CHAR} CACHE INTERNAL "sizeof 'uint8_t'")
ELSEIF (HAVE_UINT8_T)
  SET (SIZEOF_UINT8_T ${UNIT8_T} CACHE INTERNAL "sizeof 'uint8_t'")
ENDIF (NOT SIZEOF_UINT8_T)

#/* Define to 1 if the system has the type `struct sockaddr_in'. */
CHECK_TYPE_SIZE ("struct sockaddr_in" STRUCT_SOCKADDR_IN)

#/* Define to 1 if the system has the type `struct sockaddr_in6'. */
CHECK_TYPE_SIZE ("struct sockaddr_in6" STRUCT_SOCKADDR_IN6)

#/* Define to 1 if the system has the type `struct sockaddr_storage'. */
CHECK_TYPE_SIZE ("struct sockaddr_storage" STRUCT_SOCKADDR_STORAGE)

#/* Define to 1 if the system has the type `uintptr_t'. */
CHECK_TYPE_SIZE (uintptr_t UINTPTR_T)

#/* Alignment of type char */
C_GET_ALIGNMENT (char c CHAR)

#/* Alignment of type bool */
C_GET_ALIGNMENT (bool cxx BOOL)

#/* Alignment of type double */
C_GET_ALIGNMENT (double c DOUBLE)

#/* Alignment of type float */
C_GET_ALIGNMENT (float c FLOAT)

#/* Alignment of type int */
C_GET_ALIGNMENT (int c INT)

#/* Alignment of type long */
C_GET_ALIGNMENT (long c LONG)

#/* Alignment of type long double */
C_GET_ALIGNMENT ("long double" c LONG_DOUBLE)

#/* Alignment of type long long */
C_GET_ALIGNMENT ("long long" c LONG_LONG)

#/* Alignment of type short */
C_GET_ALIGNMENT (short c SHORT)

#/* Alignment of type void * */
C_GET_ALIGNMENT ("void *" c VOID_P)

#/* Alignment of type wchar_t */
C_GET_ALIGNMENT (wchar_t c WCHAR_T)

#/* Whether the C compiler supports "bool" without any other help (such as
#   <stdbool.h>) */
INCLUDE(c_check_bool)
IF (COMPILER_SUPPORT_BOOL)
  SET (OMPI_NEED_C_BOOL 0)
ELSE (COMPILER_SUPPORT_BOOL)
  SET (OMPI_NEED_C_BOOL 1)  
ENDIF (COMPILER_SUPPORT_BOOL)

#/* Define to `__inline__' or `__inline' if that's what the C compiler
#   calls it, or to nothing if 'inline' is not supported under any name.  */
CHECK_C_INLINE()
IF(HAVE_INLINE)
  SET(inline 1)
ENDIF(HAVE_INLINE)

###################################################################
#                          Check C++ types                        #
###################################################################

#
#/* Whether C++ compiler supports DEC style inline assembly */
#/* #undef OMPI_CXX_DEC_INLINE_ASSEMBLY */
#
#/* Whether C++ compiler supports GCC style inline assembly */
#/* #undef OMPI_CXX_GCC_INLINE_ASSEMBLY */
#
#/* Whether C++ compiler supports __builtin_expect */
#/* #undef OMPI_CXX_HAVE_BUILTIN_EXPECT */
#
#/* Whether C++ compiler supports __builtin_prefetch */
#/* #undef OMPI_CXX_HAVE_BUILTIN_PREFETCH */
#
#/* Whether a const_cast on a 2-d array will work with the C++ compiler */
#/* #undef OMPI_CXX_SUPPORTS_2D_CONST_CAST */
#
#/* Whether C++ compiler supports XLC style inline assembly */
#/* #undef OMPI_CXX_XLC_INLINE_ASSEMBLY */
#
#/* Whether C compiler supports DEC style inline assembly */
#/* #undef OMPI_C_DEC_INLINE_ASSEMBLY */
#
#/* Whether C compiler supports GCC style inline assembly */
#/* #undef OMPI_C_GCC_INLINE_ASSEMBLY */
#
#/* Whether C compiler supports __builtin_expect */
#/* #undef OMPI_C_HAVE_BUILTIN_EXPECT */
#
#/* Whether C compiler supports __builtin_prefetch */
#/* #undef OMPI_C_HAVE_BUILTIN_PREFETCH */
#
#/* Whether C compiler supports -fvisibility */
#/* #undef OMPI_C_HAVE_VISIBILITY */
SET(OMPI_C_HAVE_VISIBILITY 1)
#
#/* Whether C compiler supports XLC style inline assembly */
#/* #undef OMPI_C_XLC_INLINE_ASSEMBLY */
#

SET(OMPI_HAVE_CXX_EXCEPTION_SUPPORT 0)

###################################################################
#                      Check Fortran 77 types                     #
###################################################################
INCLUDE(setup_F77)
INCLUDE(f77_check)

# This allows us to mark bogus types, but still have them be a valid
# [sentinel] value
SET(ompi_fortran_bogus_type_t "int")

# We want to set the #define's for all of these, so invoke the macros
# regardless of whether we have F77 support or not.
OMPI_F77_CHECK("LOGICAL" "yes" "char;int;long long;long" "-1")
OMPI_F77_CHECK("LOGICAL*1" "yes" "char;short;int;long long;long" "1")
OMPI_F77_CHECK("LOGICAL*2" "yes" "short;int;long long;long" "2")
OMPI_F77_CHECK("LOGICAL*4" "yes" "int;long long;long" "4")
OMPI_F77_CHECK("LOGICAL*8" "yes" "int;long long;long" "8")
OMPI_F77_CHECK("INTEGER" "yes" "int;long long;long" "-1")
OMPI_F77_CHECK("INTEGER*1" "no" "char;short;int;long long;long" "1")
OMPI_F77_CHECK("INTEGER*2" "no" "short;int;long long;long" "2")
OMPI_F77_CHECK("INTEGER*4" "no" "int;long long;long" "4")
OMPI_F77_CHECK("INTEGER*8" "no" "int;long long;long" "8")
OMPI_F77_CHECK("INTEGER*16" "no" "int;long long;long" "16")

OMPI_F77_CHECK("REAL" "yes" "float;double;long double" "-1")
OMPI_F77_CHECK("REAL*2" "no" "float;double;long double" "2")
OMPI_F77_CHECK("REAL*4" "no" "float;double;long double" "4")
OMPI_F77_CHECK("REAL*8" "no" "float;double;long double" "8")
OMPI_F77_CHECK("REAL*16" "no" "float;double;long double" "16")
OMPI_F77_CHECK("DOUBLE PRECISION" "yes" "float;double;long double" "-1")

OMPI_F77_CHECK("COMPLEX" "yes" "" "-1")

# The complex*N tests are a bit different (note: the complex tests are
# the same as all the rest, because complex is a composite of two
# reals, which we *have* to have.  It's only the complex*N tests that
# are different).  The fortran complex types are composites of the
# real*(N/2) types.  So for us to support complex*N, two conditions
# must be true:
#
# a) we must support real*(N/2) (i.e., compiler supports it and we
#    have a back-end C type for it)
# b) compiler supports complex*N

OMPI_F77_CHECK("COMPLEX*8" "no" "" "8")
OMPI_F77_CHECK("COMPLEX*16" "no" "" "16")
OMPI_F77_CHECK("COMPLEX*32" "no" "" "32")

include(f77_check_real16_c_equiv)
OMPI_F77_CHECK_REAL16_C_EQUIV()

# Regardless of whether we have fortran bindings, or even a fortran
# compiler, get the max value for a fortran MPI handle (this macro
# handles the case where we don't have a fortran compiler).

#OMPI_F77_GET_FORTRAN_HANDLE_MAX

#
# Check for Fortran compilers value of TRUE and for the correct assumption
# on LOGICAL for conversion into what C considers to be a true value
#
#OMPI_F77_GET_VALUE_TRUE
#OMPI_F77_CHECK_LOGICAL_ARRAY

#
# There are 2 layers to the MPI f77 layer. The only extra thing that
# determine f77 bindings is that fortran can be disabled by user. In
# such cases, we need to not build the target at all.  One layer
# generates MPI_f77* bindings. The other layer generates PMPI_f77*
# bindings. The following conditions determine whether each (or both)
# these layers are built.
#
# Superceeding clause:
#   - fortran77 bindings should be enabled, else everything is
#     disabled
# 1. MPI_f77* bindings are needed if:
#   - Profiling is not required
#   - Profiling is required but weak symbols are not
#     supported
# 2. PMPI_* bindings are needed if profiling is required.  Hence we
# define 2 conditionals which tell us whether each of these layers
# need to be built or NOT
#

IF(NOT WANT_MPI_PROFILING OR OMPI_PROFILING_COMPILE_SEPARATELY AND  OMPI_WANT_F77_BINDINGS)
  SET(WANT_MPI_F77_BINDINGS_LAYER 1)
ELSE(NOT WANT_MPI_PROFILING OR OMPI_PROFILING_COMPILE_SEPARATELY AND  OMPI_WANT_F77_BINDINGS)
  SET(WANT_MPI_F77_BINDINGS_LAYER 0) 
ENDIF(NOT WANT_MPI_PROFILING OR OMPI_PROFILING_COMPILE_SEPARATELY AND  OMPI_WANT_F77_BINDINGS)

IF(WANT_MPI_PROFILINGAND AND OMPI_WANT_F77_BINDINGS)
  SET(WANT_MPI_F77_BINDINGS_LAYER 1)
ELSE(WANT_MPI_PROFILINGAND AND OMPI_WANT_F77_BINDINGS)
  SET(WANT_MPI_F77_BINDINGS_LAYER 0) 
ENDIF(WANT_MPI_PROFILINGAND AND OMPI_WANT_F77_BINDINGS)

#/* Max handle value for fortran MPI handles, effectively min(INT_MAX, max
#   fortran INTEGER value) */
#/* #undef OMPI_FORTRAN_HANDLE_MAX */
# Need to be fixed.
IF(WIN32)
  SET (OMPI_FORTRAN_HANDLE_MAX "2147483647")
ENDIF(WIN32)
#
#/* Fortran value for LOGICAL .TRUE. value */
#/* #undef OMPI_FORTRAN_VALUE_TRUE */
# Need to be fixed.
IF(WIN32)
  SET (OMPI_FORTRAN_VALUE_TRUE 0)
ENDIF(WIN32)


###################################################################
#                      Check Fortran 90 types                     #
###################################################################

#/* OMPI underlying F90 compiler */
#/* #undef OMPI_F90 */
  
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_COMPLEX */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_COMPLEX16 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_COMPLEX32 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_COMPLEX8 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_DOUBLE_COMPLEX */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_DOUBLE_PRECISION */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_INTEGER */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_INTEGER1 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_INTEGER16 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_INTEGER2 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_INTEGER4 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_INTEGER8 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_LOGICAL */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_REAL */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_REAL16 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_REAL2 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_REAL4 */
#
#/* Whether we have Fortran 90 $ofc_fortran_type or not */
#/* #undef OMPI_HAVE_F90_REAL8 */
#

###########################################################################################
#                                      end
###########################################################################################


#/* Define to equivalent of C99 restrict keyword, or to nothing if this is not
#   supported. Do not define if restrict is supported directly. */
IF(WIN32)
  SET(restrict "")
ENDIF(WIN32)


#/* Define if /dev/poll is available */
IF(EXISTS /dev/poll)
  SET (HAVE_DEVPOLL 1)
ENDIF(EXISTS /dev/poll)

#/* Define to 1 if you have the `nsl' library (-lnsl). */
IF(EXISTS /lib/libnsl.so.0 OR /lib/libnsl.so.1)
  SET (HAVE_LIBNSL 1)
ENDIF(EXISTS /lib/libnsl.so.0 OR /lib/libnsl.so.1)

#/* Define to 1 if you have the `socket' library (-lsocket). */
IF(EXISTS /lib/libsocket.so.0 OR /lib/libsocket.so.1)
  SET (HAVE_LIBSOCKET 1)
ENDIF(EXISTS /lib/libsocket.so.0 OR /lib/libsocket.so.1)

#/* Define to 1 if you have the `util' library (-lutil). */
IF(EXISTS /lib/libutil.so.0 OR /lib/libutil.so.1)
  SET (HAVE_LIBUTIL 1)
ENDIF(EXISTS /lib/libutil.so.0 OR /lib/libutil.so.1)

#/* Header to include for memcpy implementation */
SET(MCA_memcpy_IMPLEMENTATION_HEADER "\"opal/mca/memcpy/base/memcpy_base_default.h\"")


#/* name of component to use for direct calls, if MCA_mtl_DIRECT_CALL is 1 */
IF(NOT MCA_mtl_DIRECT_CALL_VALUE)
  SET(MCA_mtl_DIRECT_CALL_COMPONENT "")
ENDIF(NOT MCA_mtl_DIRECT_CALL_VALUE)

#/* Header mtl includes to be direct called */
IF(NOT MCA_mtl_DIRECT_CALL_VALUE)
  SET(MCA_mtl_DIRECT_CALL_HEADER "\"\"")
ENDIF(NOT MCA_mtl_DIRECT_CALL_VALUE)


#/* name of component to use for direct calls, if MCA_pml_DIRECT_CALL is 1 */
IF(NOT MCA_pml_DIRECT_CALL_VALUE)
  SET(MCA_pml_DIRECT_CALL_COMPONENT "")
ENDIF(NOT MCA_pml_DIRECT_CALL_VALUE)

#/* Header pml includes to be direct called */
IF(NOT MCA_pml_DIRECT_CALL_VALUE)
  SET(MCA_pml_DIRECT_CALL_HEADER  "\"\"")
ENDIF(NOT MCA_pml_DIRECT_CALL_VALUE)

#/* Header to include for timer implementation */
IF(WIN32)
  SET(MCA_timer_IMPLEMENTATION_HEADER 1)
  SET(MCA_TIMER_HEADER "\"opal/mca/timer/windows/timer_windows.h\"")
ENDIF(WIN32)


#
#/* OMPI architecture string */
#/* #undef OMPI_ARCH */
#
#/* Assembly align directive expects logarithmic value */
#/* #undef OMPI_ASM_ALIGN_LOG */
#
#/* Assembly directive for exporting symbols */
#/* #undef OMPI_ASM_GLOBAL */
#
#/* Assembly prefix for gsym labels */
#/* #undef OMPI_ASM_GSYM */
#
#/* Assembly suffix for labels */
#/* #undef OMPI_ASM_LABEL_SUFFIX */
#
#/* Assembly prefix for lsym labels */
#/* #undef OMPI_ASM_LSYM */
#
#/* Do we need to give a .size directive */
#/* #undef OMPI_ASM_SIZE */
#
#/* Whether we can do 64bit assembly operations or not. Should not be used
#   outside of the assembly header files */
#/* #undef OMPI_ASM_SUPPORT_64BIT */
#
#/* Assembly directive for setting text section */
#/* #undef OMPI_ASM_TEXT */
#
#/* How to set function type in .type directive */
#/* #undef OMPI_ASM_TYPE */
#
#/* Architecture type of assembly to use for atomic operations */
#/* #undef OMPI_ASSEMBLY_ARCH */
IF(WIN32)
  SET (OMPI_ASSEMBLY_ARCH "OMPI_WINDOWS")
ENDIF(WIN32)

#
#/* Format of assembly file */
#/* #undef OMPI_ASSEMBLY_FORMAT */
#
#/* OMPI underlying C compiler */
#/* #undef OMPI_CC */
IF (CMAKE_C_COMPILER)
  SET (OMPI_CC ${CMAKE_C_COMPILER})
ENDIF (CMAKE_C_COMPILER)

#
#/* OMPI underlying C++ compiler */
#/* #undef OMPI_CXX */
IF (CMAKE_CXX_COMPILER)
  SET (OMPI_CXX ${CMAKE_CXX_COMPILER})
ENDIF (CMAKE_CXX_COMPILER)



#

#
#

#

#
#/* Greek - alpha, beta, etc - release number of Open MPI */
#/* #undef OMPI_GREEK_VERSION */

#
#/* Whether there is an atomic assembly file available */
#/* #undef OMPI_HAVE_ASM_FILE */
#
#/* Whether your compiler has __attribute__ or not */
#/* #undef OMPI_HAVE_ATTRIBUTE */
#
#/* Whether your compiler has __attribute__ aligned or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_ALIGNED */
#
#/* Whether your compiler has __attribute__ always_inline or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_ALWAYS_INLINE */
#
#/* Whether your compiler has __attribute__ const or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_CONST */
#
#/* Whether your compiler has __attribute__ deprecated or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_DEPRECATED */
#
#/* Whether your compiler has __attribute__ format or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_FORMAT */
#
#/* Whether your compiler has __attribute__ malloc or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_MALLOC */
#
#/* Whether your compiler has __attribute__ may_alias or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_MAY_ALIAS */
#
#/* Whether your compiler has __attribute__ nonnull or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_NONNULL */
#
#/* Whether your compiler has __attribute__ noreturn or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_NORETURN */
#
#/* Whether your compiler has __attribute__ no_instrument_function or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION */
#
#/* Whether your compiler has __attribute__ packed or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_PACKED */
#
#/* Whether your compiler has __attribute__ pure or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_PURE */
#
#/* Whether your compiler has __attribute__ sentinel or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_SENTINEL */
#
#/* Whether your compiler has __attribute__ unused or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_UNUSED */
#
#/* Whether your compiler has __attribute__ visibility or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_VISIBILITY */
#
#/* Whether your compiler has __attribute__ warn unused result or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT */
#
#/* Whether your compiler has __attribute__ weak alias or not */
#/* #undef OMPI_HAVE_ATTRIBUTE_WEAK_ALIAS */
#
#/* whether qsort is broken or not */
#/* #undef OMPI_HAVE_BROKEN_QSORT */
#

#/* Do we have POSIX threads */
#/* #undef OMPI_HAVE_POSIX_THREADS */
SET(OMPI_HAVE_POSIX_THREADS 0)
#
#/* Do not use outside of mpi.h. Define to 1 if the system has the type 'long
#   long'. */
IF(HAVE_LONG_LONG)
  SET(OMPI_HAVE_LONG_LONG 1)
ENDIF(HAVE_LONG_LONG)

#
#/* If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK */
#/* #undef OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK */
#
#/* If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK_NP */
#/* #undef OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP */
#
#/* Whether we have the sa_len struct in <sys/socket.h> or not */
#/* #undef OMPI_HAVE_SA_LEN */
#
#/* Whether we have SA_RESTART in <signal.h> or not */
#/* #undef OMPI_HAVE_SA_RESTART */
#
#/* Do we have native Solaris threads */
#/* #undef OMPI_HAVE_SOLARIS_THREADS */
SET(OMPI_HAVE_SOLARIS_THREADS 0)
#
#/* Whether we have __va_copy or not */
#/* #undef OMPI_HAVE_UNDERSCORE_VA_COPY */
#
#/* Whether we have va_copy or not */
#/* #undef OMPI_HAVE_VA_COPY */
#
#/* Wehther we have weak symbols or not */
#/* #undef OMPI_HAVE_WEAK_SYMBOLS */

# We don't support weak symbols on windows
SET(OMPI_HAVE_WEAK_SYMBOLS 0)

#
#/* Number of arguments to ibv_create_cq */
#/* #undef OMPI_IBV_CREATE_CQ_ARGS */
#
#/* Major release number of Open MPI */
#/* #undef OMPI_MAJOR_VERSION */
#
#/* Version of the GM API to use */
#/* #undef OMPI_MCA_BTL_GM_API_VERSION */
#
#/* The GM build has or not a broker gm_get function */
#/* #undef OMPI_MCA_BTL_GM_GET_BROKEN */
#
#/* Whether we have get_get() or not */
#/* #undef OMPI_MCA_BTL_GM_HAVE_RDMA_GET */
#
#/* Whether we have gm_put() or gm_directed_send_with_callback() */
#/* #undef OMPI_MCA_BTL_GM_HAVE_RDMA_PUT */
#
#/* The OS support or not the virtal page registration */
#/* #undef OMPI_MCA_BTL_GM_SUPPORT_REGISTERING */
#
#/* Whether any opal memory mca components were found */
#/* #undef OMPI_MEMORY_HAVE_COMPONENT */
#
#/* Trigger callbacks on sbrk instead of malloc or free */
#/* #undef OMPI_MEMORY_PTMALLOC2_OPT_SBRK */
#
#/* Minor release number of Open MPI */
#/* #undef OMPI_MINOR_VERSION */
#
#/* Type of MPI_Offset -- has to be defined here and typedef'ed later because
#   mpi.h does not get AC SUBST's */
#/* #undef OMPI_MPI_OFFSET_TYPE */
SET (OMPI_MPI_OFFSET_TYPE 1)
SET (OMPI_MPI_OFFSET_TYPE_STRING "long long")
#
#/* Version of the MX API to use */
#/* #undef OMPI_MX_API_VERSION */
#
#
#/* MPI datatype corresponding to MPI_Offset */
#/* #undef OMPI_OFFSET_DATATYPE */
#
#/* Use the Cray XT-3 implementation of Portals */
#/* #undef OMPI_PORTALS_CRAYXT3 */
#
#/* Does Portals send a PTL_EVENT_UNLINK event */
#/* #undef OMPI_PORTALS_HAVE_EVENT_UNLINK */
#
#/* Use the UTCP reference implementation of Portals */
#/* #undef OMPI_PORTALS_UTCP */
#
#/* Whether r notation is used for ppc registers */
#/* #undef OMPI_POWERPC_R_REGISTERS */
#


#/* type to use for ptrdiff_t */
#/* #undef OMPI_PTRDIFF_TYPE */
IF(HAVE_PTRDIFF_T)
  SET(OMPI_PTRDIFF_TYPE "ptrdiff_t")
ENDIF(HAVE_PTRDIFF_T)
  

#
#/* Release release number of Open MPI */
#/* #undef OMPI_RELEASE_VERSION */
#
#/* whether to use cnos_barrier or not */
#/* #undef OMPI_RML_CNOS_HAVE_BARRIER */
#
#
#/* Do threads have different pids (pthreads on linux) */
#/* #undef OMPI_THREADS_HAVE_DIFFERENT_PIDS */
#
#/* Whether to use <stdbool.h> or not */
#/* #undef OMPI_USE_STDBOOL_H */
#
#/* Complete release number of Open MPI */
#/* #undef OMPI_VERSION */
##


#/* Greek - alpha, beta, etc - release number of Open Portable Access Layer */
#/* #undef OPAL_GREEK_VERSION */
# Defined in upper level.

#
#/* Whether we have the _SC_NPROCESSORS_ONLN */
#/* #undef OPAL_HAVE__SC_NPROCESSORS_ONLN */
#
#/* Major release number of Open Portable Access Layer */
#/* #undef OPAL_MAJOR_VERSION */
# Defined in upper level.

#
#/* Minor release number of Open Portable Access Layer */
#/* #undef OPAL_MINOR_VERSION */
# Defined in upper level.

#/* package/branding string for Open MPI */
#/* #undef OPAL_PACKAGE_STRING
SET(OPAL_PACKAGE_STRING "\"Open MPI $ENV{USERNAME}@$ENV{COMPUTERNAME} Distribution\"")

#
#/* Release release number of Open Portable Access Layer */
#/* #undef OPAL_RELEASE_VERSION */
# Defined in upper level.

#
#/* Complete release number of Open Portable Access Layer */
#/* #undef OPAL_VERSION */
# Defined in upper level.

#
#/* Specific ps command to use in orte-clean */
#/* #undef ORTE_CLEAN_PS_CMD */

#
#/* Greek - alpha, beta, etc - release number of Open Run-Time Environment */
#/* #undef ORTE_GREEK_VERSION */
# Defined in upper level.

#
#/* Major release number of Open Run-Time Environment */
#/* #undef ORTE_MAJOR_VERSION */
# Defined in upper level.

#
#/* Minor release number of Open Run-Time Environment */
#/* #undef ORTE_MINOR_VERSION */
# Defined in upper level.

#
#/* Release release number of Open Run-Time Environment */
#/* #undef ORTE_RELEASE_VERSION */
# Defined in upper level.

#
#/* Complete release number of Open Run-Time Environment */
#/* #undef ORTE_VERSION */
# Defined in upper level.

#
#/* Define to the address where bug reports for this package should be sent. */
#/* #undef PACKAGE_BUGREPORT */
SET (PACKAGE_BUGREPORT "\"http://www.open-mpi.org/community/help/\"")
#
#/* Define to the full name of this package. */
#/* #undef PACKAGE_NAME */
SET (PACKAGE_NAME "Open MPI")
#
#/* Define to the full name and version of this package. */
#/* #undef PACKAGE_STRING */
SET (PACKAGE_STRING "${PACKAGE_NAME} ${VERSION_STRING}")
#
#/* Define to the one symbol short name of this package. */
#/* #undef PACKAGE_TARNAME */
SET (PACKAGE_TARNAME "openmpi")
#
#/* Define to the version of this package. */
#/* #undef PACKAGE_VERSION */
SET (PACKAGE_VERSION ${VERSION_STRING})
#
#/* The PLPA symbol prefix */
#/* #undef PLPA_SYM_PREFIX */
#
#/* The PLPA symbol prefix in all caps */
#/* #undef PLPA_SYM_PREFIX_CAPS */
#
#
#/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#/* #undef TIME_WITH_SYS_TIME */
#
#/* Define to 1 if your processor stores words with the most significant byte
#   first (like Motorola and SPARC, unlike Intel and VAX). */
#/* #undef WORDS_BIGENDIAN */
#
#/* Additional CFLAGS to pass through the wrapper compilers */
#/* #undef WRAPPER_EXTRA_CFLAGS */
#
#/* Additional CXXFLAGS to pass through the wrapper compilers */
#/* #undef WRAPPER_EXTRA_CXXFLAGS */
#
#/* Additional FCFLAGS to pass through the wrapper compilers */
#/* #undef WRAPPER_EXTRA_FCFLAGS */
#
#/* Additional FFLAGS to pass through the wrapper compilers */
#/* #undef WRAPPER_EXTRA_FFLAGS */
#
#/* Additional LDFLAGS to pass through the wrapper compilers */
#/* #undef WRAPPER_EXTRA_LDFLAGS */
#
#/* Additional LIBS to pass through the wrapper compilers */
#/* #undef WRAPPER_EXTRA_LIBS */
#
#/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
#   `char[]'. */
#/* #undef YYTEXT_POINTER */
#
#/* Enable GNU extensions on systems that have them.  */
##ifndef _GNU_SOURCE
#/* #undef _GNU_SOURCE */
##endif
#
#/* Emulated value */
#/* #undef __NR_sched_getaffinity */
#
#/* Emulated value */
#/* #undef __NR_sched_setaffinity */
#
#
#/* A bogus type that allows us to have sentinel type values that are still
#   valid */
#/* #undef ompi_fortran_bogus_type_t */
#IF(WIN32)
#  SET(ompi_fortran_bogus_type_t 1)
#  SET(ompi_fortran_bogus_type_t_STRING "int")
#ENDIF(WIN32)
#
#/* C type corresponding to Fortran 77 COMPLEX*16 */
#/* #undef ompi_fortran_complex16_t */
#IF(WIN32)
#  SET(ompi_fortran_complex16_t 1)
#  SET(ompi_fortran_complex16_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 COMPLEX*32 */
##/* #undef ompi_fortran_complex32_t */
#IF(WIN32)
#  SET(ompi_fortran_complex32_t 1)
#  SET(ompi_fortran_complex32_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 COMPLEX*8 */
##/* #undef ompi_fortran_complex8_t */
#IF(WIN32)
#  SET(ompi_fortran_complex8_t 1)
#  SET(ompi_fortran_complex8_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 COMPLEX */
##/* #undef ompi_fortran_complex_t */
#IF(WIN32)
#  SET(ompi_fortran_complex_t 1)
#  SET(ompi_fortran_complex_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 DOUBLE PRECISION */
##/* #undef ompi_fortran_double_precision_t */
#IF(WIN32)
#  SET(ompi_fortran_double_precision_t 1)
#  SET(ompi_fortran_double_precision_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 INTEGER*16 */
##/* #undef ompi_fortran_integer16_t */
#IF(WIN32)
#  SET(ompi_fortran_integer16_t 1)
#  SET(ompi_fortran_integer16_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 INTEGER*1 */
##/* #undef ompi_fortran_integer1_t */
#IF(WIN32)
#  SET(ompi_fortran_integer1_t 1)
#  SET(ompi_fortran_integer1_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 INTEGER*2 */
##/* #undef ompi_fortran_integer2_t */
#IF(WIN32)
#  SET(ompi_fortran_integer2_t 1)
#  SET(ompi_fortran_integer2_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 INTEGER*4 */
##/* #undef ompi_fortran_integer4_t */
#IF(WIN32)
#  SET(ompi_fortran_integer4_t 1)
#  SET(ompi_fortran_integer4_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 INTEGER*8 */
##/* #undef ompi_fortran_integer8_t */
#IF(WIN32)
#  SET(ompi_fortran_integer8_t 1)
#  SET(ompi_fortran_integer8_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 INTEGER */
##/* #undef ompi_fortran_integer_t */
#IF(WIN32)
#  SET(ompi_fortran_integer_t 1)
#  SET(ompi_fortran_integer_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 LOGICAL */
##/* #undef ompi_fortran_logical_t */
#IF(WIN32)
#  SET(ompi_fortran_logical_t 1)
#  SET(ompi_fortran_logical_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 REAL*16 */
##/* #undef ompi_fortran_real16_t */
#IF(WIN32)
#  SET(ompi_fortran_real16_t 1)
#  SET(ompi_fortran_real16_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 REAL*2 */
##/* #undef ompi_fortran_real2_t */
#IF(WIN32)
#  SET(ompi_fortran_real2_t 1)
#  SET(ompi_fortran_real2_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 REAL*4 */
##/* #undef ompi_fortran_real4_t */
#IF(WIN32)
#  SET(ompi_fortran_real4_t 1)
#  SET(ompi_fortran_real4_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 REAL*8 */
##/* #undef ompi_fortran_real8_t */
#IF(WIN32)
#  SET(ompi_fortran_real8_t 1)
#  SET(ompi_fortran_real8_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
##
##/* C type corresponding to Fortran 77 REAL */
##/* #undef ompi_fortran_real_t */
#IF(WIN32)
#  SET(ompi_fortran_real_t 1)
#  SET(ompi_fortran_real_t_STRING "ompi_fortran_bogus_type_t")
#ENDIF(WIN32)
