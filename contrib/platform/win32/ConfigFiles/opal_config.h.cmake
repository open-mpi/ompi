/* opal/include/opal_config.h.in.  Generated from configure.ac by autoheader.  */

/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * Function: - OS, CPU and compiler dependent configuration 
 */

#ifndef OPAL_CONFIG_H
#define OPAL_CONFIG_H


#define OMPI_CONFIGURE_HOST "$ENV{COMPUTERNAME}"

#define OMPI_CONFIGURE_USER "$ENV{USERNAME}"

#define OMPI_BUILD_USER "$ENV{USERNAME}"

#define OMPI_BUILD_HOST "$ENV{COMPUTERNAME}"

#define OMPI_BUILD_CFLAGS ${OMPI_BUILD_CFLAGS}

#define OMPI_BUILD_CXXFLAGS ${OMPI_BUILD_CXXFLAGS}

#define OMPI_BUILD_FFLAGS ${OMPI_BUILD_FFLAGS}

#define OMPI_BUILD_FCFLAGS ${OMPI_BUILD_FCFLAGS}

#define OMPI_BUILD_LDFLAGS ${OMPI_BUILD_LDFLAGS}

#define OMPI_BUILD_LIBS ${OMPI_BUILD_LIBS}

#define OMPI_CC_ABSOLUTE "${OMPI_CC}"

#define OMPI_CXX_ABSOLUTE  "${OMPI_CXX}"

#define OMPI_F77_ABSOLUTE ${OMPI_F77_ABSOLUTE}

#define OMPI_F90_ABSOLUTE ${OMPI_F90_ABSOLUTE}

#define OMPI_F90_BUILD_SIZE ${OMPI_F90_BUILD_SIZE}

#define OMPI_RELEASE_DATE ${RELEASE_DATE}

#define OPAL_RELEASE_DATE ${RELEASE_DATE}

#define ORTE_RELEASE_DATE ${RELEASE_DATE}

/* Define to 1 if you have the <winsock2.h> header file. */
#cmakedefine HAVE_WINSOCK2_H 1

/* Define to 1 if you have the <windows.h> header file. */
#cmakedefine HAVE_WINDOWS_H 1

/* Define to 1 if you have the <aio.h> header file. */
#cmakedefine HAVE_AIO_H 1

/* Define to 1 if you have the <alloca.h> header file. */
#cmakedefine HAVE_ALLOCA_H 1

/* Define to 1 if you have the <arpa/inet.h> header file. */
#cmakedefine HAVE_ARPA_INET_H 1

/* Define to 1 if you have the `asprintf' function. */
#cmakedefine HAVE_ASPRINTF 1

/* Define to 1 if you have the `backtrace' function. */
#cmakedefine HAVE_BACKTRACE 1

/* Define to 1 if you have the `ceil' function. */
#cmakedefine HAVE_CEIL 1

/* Define to 1 if you have the `cnos_pm_barrier' function. */
#cmakedefine HAVE_CNOS_PM_BARRIER 1

/* Define to 1 if you have the <crt_externs.h> header file. */
#cmakedefine HAVE_CRT_EXTERNS_H 1

/* Define to 1 if you have the declaration of `AF_UNSPEC', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_AF_UNSPEC 1

/* Define to 1 if you have the declaration of `PF_UNSPEC', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_PF_UNSPEC 1

/* Define to 1 if you have the declaration of `AF_INET6', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_AF_INET6 1

/* Define to 1 if you have the declaration of `PF_INET6', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_PF_INET6 1

/* Define to 1 if you have the declaration of `IBV_EVENT_CLIENT_REREGISTER',
   and to 0 if you don't. */
#cmakedefine HAVE_DECL_IBV_EVENT_CLIENT_REREGISTER 1

/* Define to 1 if you have the declaration of `RLIMIT_NPROC', and to 0 if you
   don't. */
#cmakedefine HAVE_DECL_RLIMIT_NPROC 1

/* Define to 1 if you have the declaration of `sbrk', and to 0 if you don't.
   */
#cmakedefine HAVE_DECL_SBRK 1

/* Define to 1 if you have the declaration of `__func__', and to 0 if you
   don't. */
#define HAVE_DECL___FUNC__ ${HAVE_DECL___FUNC__}

/* Define if /dev/poll is available */
#cmakedefine HAVE_DEVPOLL 1

/* Define to 1 if you have the <dirent.h> header file. */
#cmakedefine HAVE_DIRENT_H 1

/* Define to 1 if you have the `dirname' function. */
#cmakedefine HAVE_DIRNAME 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#cmakedefine HAVE_DLFCN_H 1

/* Define to 1 if you have the `dlsym' function. */
#cmakedefine HAVE_DLSYM 1

/* Define if your system supports the epoll system calls */
#cmakedefine HAVE_EPOLL

/* Define to 1 if you have the `epoll_ctl' function. */
#cmakedefine HAVE_EPOLL_CTL 1

/* Define to 1 if you have the <err.h> header file. */
#cmakedefine HAVE_ERR_H 1

/* Define to 1 if you have the <execinfo.h> header file. */
#cmakedefine HAVE_EXECINFO_H 1

/* Define to 1 if you have the `execve' function. */
#cmakedefine HAVE_EXECVE 1

/* Define to 1 if you have the `fcntl' function. */
#cmakedefine HAVE_FCNTL 1

/* Define to 1 if you have the <fcntl.h> header file. */
#cmakedefine HAVE_FCNTL_H 1

/* Define to 1 if you have the `fork' function. */
#cmakedefine HAVE_FORK 1

/* Define to 1 if you have the `getpwuid' function. */
#cmakedefine HAVE_GETPWUID 1

/* Define to 1 if you have the `gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <grp.h> header file. */
#cmakedefine HAVE_GRP_H 1

/* Define to 1 if you have the `htonl' function. */
#cmakedefine HAVE_HTONL 1

/* Define to 1 if you have the `htons' function. */
#cmakedefine HAVE_HTONS 1

/* Define to 1 if you have the `ibv_fork_init' function. */
#cmakedefine HAVE_IBV_FORK_INIT 1

/* Define to 1 if you have the `ibv_get_device_list' function. */
#cmakedefine HAVE_IBV_GET_DEVICE_LIST 1

/* Define to 1 if you have the `ibv_resize_cq' function. */
#cmakedefine HAVE_IBV_RESIZE_CQ 1

/* Define to 1 if you have the <ifaddrs.h> header file. */
#cmakedefine HAVE_IFADDRS_H 1

/* Define to 1 if the system has the type `int16_t'. */
#cmakedefine HAVE_INT16_T 1

/* Define to 1 if the system has the type `int32_t'. */
#cmakedefine HAVE_INT32_T 1

/* Define to 1 if the system has the type `int64_t'. */
#cmakedefine HAVE_INT64_T 1

/* Define to 1 if the system has the type `int8_t'. */
#cmakedefine HAVE_INT8_T 1

/* Whether we support 32 bits atomic operations on Windows */
#cmakedefine HAVE_INTERLOCKEDCOMPAREEXCHANGE 1

/* Whether we support 64 bits atomic operations on Windows */
#cmakedefine HAVE_INTERLOCKEDCOMPAREEXCHANGE64 1

/* Whether we support 32 bits atomic operations on Windows */
#cmakedefine HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE 1

/* Whether we support 32 bits atomic operations on Windows */
#cmakedefine HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE 1

/* Define to 1 if the system has the type `intptr_t'. */
#cmakedefine HAVE_INTPTR_T 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define to 1 if you have the `isatty' function. */
#cmakedefine HAVE_ISATTY 1

/* Define to 1 if you have the `killrank' function. */
#cmakedefine HAVE_KILLRANK 1

/* Define to 1 if you have the `kqueue' function. */
#cmakedefine HAVE_KQUEUE 1

/* Define to 1 if you have the <libcr.h> header file. */
#cmakedefine HAVE_LIBCR_H 1

/* Define to 1 if you have the <libgen.h> header file. */
#cmakedefine HAVE_LIBGEN_H 1

/* Define to 1 if you have the `nsl' library (-lnsl). */
#cmakedefine HAVE_LIBNSL 1

/* Define to 1 if you have the `socket' library (-lsocket). */
#cmakedefine HAVE_LIBSOCKET 1

/* Define to 1 if you have the `util' library (-lutil). */
#cmakedefine HAVE_LIBUTIL 1

/* Define to 1 if you have the <libutil.h> header file. */
#cmakedefine HAVE_LIBUTIL_H 1

/* Define to 1 if you have the <libxcpu.h> header file. */
#cmakedefine HAVE_LIBXCPU_H 1

/* Define to 1 if the system has the type `long double'. */
#cmakedefine HAVE_LONG_DOUBLE 1

/* Define to 1 if the system has the type `long long'. */
#cmakedefine HAVE_LONG_LONG 1

/* Define to 1 if you have the <mach/mach_time.h> header file. */
#cmakedefine HAVE_MACH_MACH_TIME_H 1

/* Define to 1 if you have the <mach/mach_vm.h> header file. */
#cmakedefine HAVE_MACH_MACH_VM_H 1

/* Define to 1 if you have the `mach_vm_read' function. */
#cmakedefine HAVE_MACH_VM_READ 1

/* Define to 1 if you have the `mach_vm_region' function. */
#cmakedefine HAVE_MACH_VM_REGION 1

/* Define to 1 if you have the <malloc.h> header file. */
#cmakedefine HAVE_MALLOC_H 1

/* Define to 1 if you have the `mallopt' function. */
#cmakedefine HAVE_MALLOPT 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define to 1 if you have the `mmap' function. */
#cmakedefine HAVE_MMAP 1

/* Define to 1 if the system has the type `mode_t'. */
#cmakedefine HAVE_MODE_T 1

/* Define to 1 if you have the <netdb.h> header file. */
#cmakedefine HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#cmakedefine HAVE_NETINET_IN_H 1

/* Define to 1 if you have the <netinet/tcp.h> header file. */
#cmakedefine HAVE_NETINET_TCP_H 1

/* Define to 1 if you have the <net/if.h> header file. */
#cmakedefine HAVE_NET_IF_H 1

/* Define to 1 if you have the `ntohl' function. */
#cmakedefine HAVE_NTOHL 1

/* Define to 1 if you have the `ntohs' function. */
#cmakedefine HAVE_NTOHS 1

/* Define to 1 if you have the `openpty' function. */
#cmakedefine HAVE_OPENPTY 1

/* Define to 1 if you have the `pipe' function. */
#cmakedefine HAVE_PIPE 1

/* Define to 1 if you have the <pmapi.h> header file. */
#cmakedefine HAVE_PMAPI_H 1

/* Define to 1 if you have the `pm_cycles' function. */
#cmakedefine HAVE_PM_CYCLES 1

/* Define to 1 if you have the `poll' function. */
#cmakedefine HAVE_POLL 1

/* Define to 1 if you have the <poll.h> header file. */
#cmakedefine HAVE_POLL_H 1

/* Define to 1 if you have the `posix_memalign' function. */
#cmakedefine HAVE_POSIX_MEMALIGN 1

/* Define to 1 if `srr0' is member of `ppc_thread_state_t'. */
#cmakedefine HAVE_PPC_THREAD_STATE_T_SRR0 1

/* Define to 1 if you have the `printstack' function. */
#cmakedefine HAVE_PRINTSTACK 1

/* Define to 1 if you have the <pthread.h> header file. */
#cmakedefine HAVE_PTHREAD_H 1

/* Define to 1 if the system has the type `ptrdiff_t'. */
#cmakedefine HAVE_PTRDIFF_T 1

/* Define to 1 if you have the `ptsname' function. */
#cmakedefine HAVE_PTSNAME 1

/* Define to 1 if you have the <pty.h> header file. */
#cmakedefine HAVE_PTY_H 1

/* Define to 1 if you have the <pwd.h> header file. */
#cmakedefine HAVE_PWD_H 1

/* Define to 1 if you have the `regcmp' function. */
#cmakedefine HAVE_REGCMP 1

/* Define to 1 if you have the `regexec' function. */
#cmakedefine HAVE_REGEXEC 1

/* Define to 1 if you have the <regex.h> header file. */
#cmakedefine HAVE_REGEX_H

/* Define to 1 if you have the `regfree' function. */
#cmakedefine HAVE_REGFREE 1

/* Define if your system supports POSIX realtime signals */
#cmakedefine HAVE_RTSIG

/* Define to 1 if you have the <sched.h> header file. */
#cmakedefine HAVE_SCHED_H 1

/* Define to 1 if you have the `sched_yield' function. */
#cmakedefine HAVE_SCHED_YIELD 1

/* Define to 1 if you have the `select' function. */
#cmakedefine HAVE_SELECT 1

/* Define if F_SETFD is defined in <fcntl.h> */
#cmakedefine HAVE_SETFD

/* Define to 1 if you have the `setsid' function. */
#cmakedefine HAVE_SETSID 1

/* Define to 1 if `si_band' is member of `siginfo_t'. */
#cmakedefine HAVE_SIGINFO_T_SI_BAND 1

/* Define to 1 if `si_fd' is member of `siginfo_t'. */
#cmakedefine HAVE_SIGINFO_T_SI_FD 1

/* Define to 1 if you have the <signal.h> header file. */
#cmakedefine HAVE_SIGNAL_H 1

/* Define to 1 if you have the `sigtimedwait' function. */
#cmakedefine HAVE_SIGTIMEDWAIT 1

/* Define to 1 if you have the `snprintf' function. */
#cmakedefine HAVE_SNPRINTF 1

/* Define to 1 if the system has the type `socklen_t'. */
#cmakedefine HAVE_SOCKLEN_T 1

/* Define to 1 if the system has the type `ssize_t'. */
#cmakedefine HAVE_SSIZE_T 1

/* Define to 1 if you have the <stdarg.h> header file. */
#cmakedefine HAVE_STDARG_H 1

/* Define to 1 if you have the <stdbool.h> header file. */
#cmakedefine HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H 1

/* Define to 1 if you have the <stropts.h> header file. */
#cmakedefine HAVE_STROPTS_H 1

/* Define to 1 if you have the `strsignal' function. */
#cmakedefine HAVE_STRSIGNAL 1

/* Define to 1 if `d_type' is member of `struct dirent'. */
#cmakedefine HAVE_STRUCT_DIRENT_D_TYPE 1

/* Define to 1 if the system has the type `struct sockaddr_in'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_IN 1

/* Define to 1 if the system has the type `struct sockaddr_in6'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_IN6 1

/* Define to 1 if the system has the type `struct sockaddr_storage'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_STORAGE 1

/* Define to 1 if you have the `syscall' function. */
#cmakedefine HAVE_SYSCALL 1

/* Define to 1 if you have the `sysconf' function. */
#cmakedefine HAVE_SYSCONF 1

/* Define to 1 if you have the `syslog' function. */
#cmakedefine HAVE_SYSLOG 1

/* Define to 1 if you have the <syslog.h> header file. */
#cmakedefine HAVE_SYSLOG_H 1

/* Define to 1 if you have the <sys/bproc_common.h> header file. */
#cmakedefine HAVE_SYS_BPROC_COMMON_H 1

/* Define to 1 if you have the <sys/bproc.h> header file. */
#cmakedefine HAVE_SYS_BPROC_H 1

/* Define to 1 if you have the <sys/devpoll.h> header file. */
#cmakedefine HAVE_SYS_DEVPOLL_H 1

/* Define to 1 if you have the <sys/epoll.h> header file. */
#cmakedefine HAVE_SYS_EPOLL_H 1

/* Define to 1 if you have the <sys/event.h> header file. */
#cmakedefine HAVE_SYS_EVENT_H 1

/* Define to 1 if you have the <sys/fcntl.h> header file. */
#cmakedefine HAVE_SYS_FCNTL_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#cmakedefine HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ipc.h> header file. */
#cmakedefine HAVE_SYS_IPC_H 1

/* Define to 1 if you have the <sys/mman.h> header file. */
#cmakedefine HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#cmakedefine HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/queue.h> header file. */
#cmakedefine HAVE_SYS_QUEUE_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#cmakedefine HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#cmakedefine HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#cmakedefine HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/sockio.h> header file. */
#cmakedefine HAVE_SYS_SOCKIO_H 1

/* Define to 1 if you have the <sys/statvfs.h> header file. */
#cmakedefine HAVE_SYS_STATVFS_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/sysctl.h> header file. */
#cmakedefine HAVE_SYS_SYSCTL_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#cmakedefine HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/tree.h> header file. */
#cmakedefine HAVE_SYS_TREE_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/uio.h> header file. */
#cmakedefine HAVE_SYS_UIO_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#cmakedefine HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have the <sys/wait.h> header file. */
#cmakedefine HAVE_SYS_WAIT_H 1

/* Define if TAILQ_FOREACH is defined in <sys/queue.h> */
#cmakedefine HAVE_TAILQFOREACH

/* Define to 1 if you have the `tcgetpgrp' function. */
#cmakedefine HAVE_TCGETPGRP 1

/* Define to 1 if you have the <termios.h> header file. */
#cmakedefine HAVE_TERMIOS_H 1

/* Define if timeradd is defined in <sys/time.h> */
#cmakedefine HAVE_TIMERADD

/* Define to 1 if you have the <time.h> header file. */
#cmakedefine HAVE_TIME_H 1

/* Define to 1 if you have the <ucontext.h> header file. */
#cmakedefine HAVE_UCONTEXT_H 1

/* Define to 1 if the system has the type `uint16_t'. */
#cmakedefine HAVE_UINT16_T 1

/* Define to 1 if the system has the type `uint32_t'. */
#cmakedefine HAVE_UINT32_T 1

/* Define to 1 if the system has the type `uint64_t'. */
#cmakedefine HAVE_UINT64_T 1

/* Define to 1 if the system has the type `uint8_t'. */
#cmakedefine HAVE_UINT8_T 1

/* Define to 1 if the system has the type `uintptr_t'. */
#cmakedefine HAVE_UINTPTR_T 1

/* Define to 1 if you have the <ulimit.h> header file. */
#cmakedefine HAVE_ULIMIT_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define to 1 if you have the <util.h> header file. */
#cmakedefine HAVE_UTIL_H 1

/* Define to 1 if you have the <utmp.h> header file. */
#cmakedefine HAVE_UTMP_H 1

/* Define to 1 if you have the `vasprintf' function. */
#cmakedefine HAVE_VASPRINTF 1

/* Define to 1 if you have the `vm_read_overwrite' function. */
#cmakedefine HAVE_VM_READ_OVERWRITE 1

/* Define to 1 if you have the `vsnprintf' function. */
#cmakedefine HAVE_VSNPRINTF 1

/* Define to 1 if you have the `waitpid' function. */
#cmakedefine HAVE_WAITPID 1

/* Define if kqueue works correctly with pipes */
#cmakedefine HAVE_WORKING_KQUEUE

/* Whether poll works for file descriptors and devices */
#cmakedefine HAVE_WORKING_POLL

/* Define if realtime signals work on pipes */
#cmakedefine HAVE_WORKING_RTSIG

/* Define to 1 if you have the `_NSGetEnviron' function. */
#cmakedefine HAVE__NSGETENVIRON 1

/* Define to 1 if you have the `__mmap' function. */
#cmakedefine HAVE___MMAP 1

/* Define to 1 if you have the `__munmap' function. */
#cmakedefine HAVE___MUNMAP 1

/* Header to include for memcpy implementation */
#cmakedefine MCA_memcpy_IMPLEMENTATION_HEADER ${MCA_memcpy_IMPLEMENTATION_HEADER}

/* Defined to 1 if mtl should use direct calls instead of components */
#define MCA_mtl_DIRECT_CALL ${MCA_mtl_DIRECT_CALL}

/* name of component to use for direct calls, if MCA_mtl_DIRECT_CALL is 1 */
#define MCA_mtl_DIRECT_CALL_COMPONENT ${MCA_mtl_DIRECT_CALL_COMPONENT}

/* Header mtl includes to be direct called */
#define MCA_mtl_DIRECT_CALL_HEADER ${MCA_mtl_DIRECT_CALL_HEADER}

/* Defined if we are using Scyld bproc or pre 3.2.0 LANL bproc */
#cmakedefine MCA_pls_bproc_scyld

/* Defined to 1 if pml should use direct calls instead of components */
#define MCA_pml_DIRECT_CALL ${MCA_pml_DIRECT_CALL}

/* name of component to use for direct calls, if MCA_pml_DIRECT_CALL is 1 */
#define MCA_pml_DIRECT_CALL_COMPONENT ${MCA_pml_DIRECT_CALL_COMPONENT} 

/* Header pml includes to be direct called */
#define MCA_pml_DIRECT_CALL_HEADER ${MCA_pml_DIRECT_CALL_HEADER}

/* Header to include for timer implementation */
#cmakedefine MCA_timer_IMPLEMENTATION_HEADER ${MCA_TIMER_HEADER}

/* Whether we want to check MPI parameters always, never, or decide at
   run-time */
#define MPI_PARAM_CHECK ${MPI_PARAM_CHECK}

/* The MX library have support for the mx_extensions.h */
#cmakedefine MX_HAVE_EXTENSIONS_H

/* MX allow to forget the completion event for mx_requests */
#cmakedefine MX_HAVE_FORGET

/* MX installation provide access to the mx_open_board and
   mx__get_mapper_state functions */
#cmakedefine MX_HAVE_MAPPER_STATE

/* MX allow registration of an unexpected handler */
#cmakedefine MX_HAVE_UNEXPECTED_HANDLER

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
#cmakedefine NO_MINUS_C_MINUS_O 1

/* Alignment of type char */
#define OMPI_ALIGNMENT_CHAR ${CHAR_ALIGNMENT}

/* Alignment of type bool */
#define OMPI_ALIGNMENT_CXX_BOOL ${BOOL_ALIGNMENT}

/* Alignment of type double */
#define OMPI_ALIGNMENT_DOUBLE ${DOUBLE_ALIGNMENT}

/* Alignment of type float */
#define OMPI_ALIGNMENT_FLOAT ${FLOAT_ALIGNMENT}

/* Alignment of Fortran 77 COMPLEX */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX ${OMPI_ALIGNMENT_FORTRAN_COMPLEX}

/* Alignment of Fortran 77 COMPLEX*16 */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX16 ${OMPI_ALIGNMENT_FORTRAN_COMPLEX16}

/* Alignment of Fortran 77 COMPLEX*32 */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX32 ${OMPI_ALIGNMENT_FORTRAN_COMPLEX32}

/* Alignment of Fortran 77 COMPLEX*8 */
#define OMPI_ALIGNMENT_FORTRAN_COMPLEX8 ${OMPI_ALIGNMENT_FORTRAN_COMPLEX8}

/* Alignment of Fortran 77 DOUBLE PRECISION */
#define OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION ${OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION}

/* Alignment of Fortran 77 INTEGER */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER ${OMPI_ALIGNMENT_FORTRAN_INTEGER}

/* Alignment of Fortran 77 INTEGER*1 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER1 ${OMPI_ALIGNMENT_FORTRAN_INTEGER1}

/* Alignment of Fortran 77 INTEGER*16 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER16 ${OMPI_ALIGNMENT_FORTRAN_INTEGER16}

/* Alignment of Fortran 77 INTEGER*2 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER2 ${OMPI_ALIGNMENT_FORTRAN_INTEGER2}

/* Alignment of Fortran 77 INTEGER*4 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER4 ${OMPI_ALIGNMENT_FORTRAN_INTEGER4}

/* Alignment of Fortran 77 INTEGER*8 */
#define OMPI_ALIGNMENT_FORTRAN_INTEGER8 ${OMPI_ALIGNMENT_FORTRAN_INTEGER8}

/* Alignment of Fortran 77 LOGICAL */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL ${OMPI_ALIGNMENT_FORTRAN_LOGICAL}

/* Alignment of Fortran 77 LOGICAL*1 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL1 ${OMPI_ALIGNMENT_FORTRAN_LOGICAL1}

/* Alignment of Fortran 77 LOGICAL*2 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL2 ${OMPI_ALIGNMENT_FORTRAN_LOGICAL2}

/* Alignment of Fortran 77 LOGICAL*4 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL4 ${OMPI_ALIGNMENT_FORTRAN_LOGICAL4}

/* Alignment of Fortran 77 LOGICAL*8 */
#define OMPI_ALIGNMENT_FORTRAN_LOGICAL8 ${OMPI_ALIGNMENT_FORTRAN_LOGICAL8}

/* Alignment of Fortran 77 REAL */
#define OMPI_ALIGNMENT_FORTRAN_REAL ${OMPI_ALIGNMENT_FORTRAN_REAL}

/* Alignment of Fortran 77 REAL*16 */
#define OMPI_ALIGNMENT_FORTRAN_REAL16 ${OMPI_ALIGNMENT_FORTRAN_REAL16}

/* Alignment of Fortran 77 REAL*2 */
#define OMPI_ALIGNMENT_FORTRAN_REAL2 ${OMPI_ALIGNMENT_FORTRAN_REAL2}

/* Alignment of Fortran 77 REAL*4 */
#define OMPI_ALIGNMENT_FORTRAN_REAL4 ${OMPI_ALIGNMENT_FORTRAN_REAL4}

/* Alignment of Fortran 77 REAL*8 */
#define OMPI_ALIGNMENT_FORTRAN_REAL8 ${OMPI_ALIGNMENT_FORTRAN_REAL8}

/* Alignment of type int */
#define OMPI_ALIGNMENT_INT ${INT_ALIGNMENT}

/* Alignment of type long */
#define OMPI_ALIGNMENT_LONG ${LONG_ALIGNMENT}

/* Alignment of type long double */
#define OMPI_ALIGNMENT_LONG_DOUBLE ${LONG_DOUBLE_ALIGNMENT}

/* Alignment of type long long */
#define OMPI_ALIGNMENT_LONG_LONG ${LONG_LONG_ALIGNMENT}

/* Alignment of type short */
#define OMPI_ALIGNMENT_SHORT ${SHORT_ALIGNMENT}

/* Alignment of type void * */
#define OMPI_ALIGNMENT_VOID_P ${VOID_P_ALIGNMENT}

/* Alignment of type wchar_t */
#define OMPI_ALIGNMENT_WCHAR ${WCHAR_T_ALIGNMENT}

/* OMPI architecture string */
#define OMPI_ARCH "${CMAKE_SYSTEM_PROCESSOR} ${CMAKE_SYSTEM}"

/* Assembly align directive expects logarithmic value */
#cmakedefine OMPI_ASM_ALIGN_LOG

/* Assembly directive for exporting symbols */
#cmakedefine OMPI_ASM_GLOBAL

/* Assembly prefix for gsym labels */
#cmakedefine OMPI_ASM_GSYM

/* Assembly suffix for labels */
#cmakedefine OMPI_ASM_LABEL_SUFFIX

/* Assembly prefix for lsym labels */
#cmakedefine OMPI_ASM_LSYM

/* Do we need to give a .size directive */
#cmakedefine OMPI_ASM_SIZE

/* Whether we can do 64bit assembly operations or not. Should not be used
   outside of the assembly header files */
#cmakedefine OMPI_ASM_SUPPORT_64BIT

/* Assembly directive for setting text section */
#cmakedefine OMPI_ASM_TEXT

/* How to set function type in .type directive */
#cmakedefine OMPI_ASM_TYPE

/* Architecture type of assembly to use for atomic operations */
#cmakedefine OMPI_ASSEMBLY_ARCH ${OMPI_ASSEMBLY_ARCH}

/* Format of assembly file */
#cmakedefine OMPI_ASSEMBLY_FORMAT

/* OMPI underlying C compiler */
#cmakedefine OMPI_CC "${OMPI_CC}"

/* OMPI underlying C++ compiler */
#cmakedefine OMPI_CXX "${OMPI_CXX}"

/* Whether C++ compiler supports DEC style inline assembly */
#cmakedefine OMPI_CXX_DEC_INLINE_ASSEMBLY

/* Whether C++ compiler supports GCC style inline assembly */
#cmakedefine OMPI_CXX_GCC_INLINE_ASSEMBLY

/* Whether C++ compiler supports __builtin_expect */
#cmakedefine OMPI_CXX_HAVE_BUILTIN_EXPECT

/* Whether C++ compiler supports __builtin_prefetch */
#cmakedefine OMPI_CXX_HAVE_BUILTIN_PREFETCH

/* Whether a const_cast on a 2-d array will work with the C++ compiler */
#cmakedefine OMPI_CXX_SUPPORTS_2D_CONST_CAST

/* Whether C++ compiler supports XLC style inline assembly */
#cmakedefine OMPI_CXX_XLC_INLINE_ASSEMBLY

/* Whether C compiler supports DEC style inline assembly */
#cmakedefine OMPI_C_DEC_INLINE_ASSEMBLY

/* Whether C compiler supports GCC style inline assembly */
#cmakedefine OMPI_C_GCC_INLINE_ASSEMBLY

/* Whether C compiler supports __builtin_expect */
#cmakedefine OMPI_C_HAVE_BUILTIN_EXPECT

/* Whether C compiler supports __builtin_prefetch */
#cmakedefine OMPI_C_HAVE_BUILTIN_PREFETCH

/* Whether C compiler supports -fvisibility */
#define OMPI_C_HAVE_VISIBILITY ${OMPI_C_HAVE_VISIBILITY}

/* Whether C compiler supports XLC style inline assembly */
#cmakedefine OMPI_C_XLC_INLINE_ASSEMBLY

/* Whether we want developer-level debugging code or not */
#define OMPI_ENABLE_DEBUG ${OMPI_ENABLE_DEBUG}

/* Enable features required for heterogeneous support */
#define OMPI_ENABLE_HETEROGENEOUS_SUPPORT ${OMPI_ENABLE_HETEROGENEOUS_SUPPORT}

/* Whether we want the memory profiling or not */
#define OMPI_ENABLE_MEM_DEBUG ${OMPI_ENABLE_MEM_DEBUG}

/* Whether we want the memory profiling or not */
#define OMPI_ENABLE_MEM_PROFILE ${OMPI_ENABLE_MEM_PROFILE}

/* Whether we want MPI profiling or not */
#define OMPI_ENABLE_MPI_PROFILING ${OMPI_ENABLE_MPI_PROFILING}

/* Whether we should enable support for multiple user threads */
#define OMPI_ENABLE_MPI_THREADS ${OMPI_ENABLE_MPI_THREADS}

/* Whether we should use progress threads rather than polling */
#define OMPI_ENABLE_PROGRESS_THREADS ${OMPI_ENABLE_PROGRESS_THREADS}

/* Whether user wants PTY support or not */
#define OMPI_ENABLE_PTY_SUPPORT ${OMPI_ENABLE_PTY_SUPPORT}

/* OMPI underlying F77 compiler */
#define OMPI_F77 "${CMAKE_Fortran_COMPILER}"

/* Whether fortran symbols are all caps or not */
#define OMPI_F77_CAPS ${OMPI_F77_CAPS}

/* Whether fortran symbols have a trailing double underscore or not */
#define OMPI_F77_DOUBLE_UNDERSCORE ${OMPI_F77_DOUBLE_UNDERSCORE}

/* Whether fortran symbols have no trailing underscore or not */
#define OMPI_F77_PLAIN ${OMPI_F77_PLAIN}

/* Whether fortran symbols have a trailing underscore or not */
#define OMPI_F77_SINGLE_UNDERSCORE ${OMPI_F77_SINGLE_UNDERSCORE}

/* OMPI underlying F90 compiler */
#define OMPI_F90 "${OMPI_F90}"

/* Max handle value for fortran MPI handles, effectively min(INT_MAX, max
   fortran INTEGER value) */
#cmakedefine OMPI_FORTRAN_HANDLE_MAX ${OMPI_FORTRAN_HANDLE_MAX}

/* Fortran value for LOGICAL .TRUE. value */
#define OMPI_FORTRAN_VALUE_TRUE ${OMPI_FORTRAN_VALUE_TRUE}

/* Greek - alpha, beta, etc - release number of Open MPI */
#cmakedefine OMPI_GREEK_VERSION "${OMPI_GREEK_VERSION_STRING}"

/* Wether we want sparse process groups */
#define OMPI_GROUP_SPARSE ${OMPI_GROUP_SPARSE}

/* Whether there is an atomic assembly file available */
#cmakedefine OMPI_HAVE_ASM_FILE

/* Whether your compiler has __attribute__ or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE

/* Whether your compiler has __attribute__ aligned or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_ALIGNED

/* Whether your compiler has __attribute__ always_inline or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_ALWAYS_INLINE

/* Whether your compiler has __attribute__ const or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_CONST

/* Whether your compiler has __attribute__ deprecated or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_DEPRECATED

/* Whether your compiler has __attribute__ format or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_FORMAT

/* Whether your compiler has __attribute__ malloc or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_MALLOC

/* Whether your compiler has __attribute__ may_alias or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_MAY_ALIAS

/* Whether your compiler has __attribute__ nonnull or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_NONNULL

/* Whether your compiler has __attribute__ noreturn or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_NORETURN

/* Whether your compiler has __attribute__ no_instrument_function or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION

/* Whether your compiler has __attribute__ packed or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_PACKED

/* Whether your compiler has __attribute__ pure or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_PURE

/* Whether your compiler has __attribute__ sentinel or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_SENTINEL

/* Whether your compiler has __attribute__ unused or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_UNUSED

/* Whether your compiler has __attribute__ visibility or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_VISIBILITY

/* Whether your compiler has __attribute__ warn unused result or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT

/* Whether your compiler has __attribute__ weak alias or not */
#cmakedefine OMPI_HAVE_ATTRIBUTE_WEAK_ALIAS

/* whether qsort is broken or not */
#cmakedefine OMPI_HAVE_BROKEN_QSORT

/* Whether or not we have compiled with C++ exceptions support */
#define OMPI_HAVE_CXX_EXCEPTION_SUPPORT ${OMPI_HAVE_CXX_EXCEPTION_SUPPORT}

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_COMPLEX

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_COMPLEX16

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_COMPLEX32

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_COMPLEX8

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_DOUBLE_COMPLEX

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_DOUBLE_PRECISION

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_INTEGER

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_INTEGER1

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_INTEGER16

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_INTEGER2

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_INTEGER4

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_INTEGER8

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_LOGICAL

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_REAL

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_REAL16

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_REAL2

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_REAL4

/* Whether we have Fortran 90 $ofc_fortran_type or not */
#cmakedefine OMPI_HAVE_F90_REAL8

/* Whether we have Fortran 77 COMPLEX or not */
#define OMPI_HAVE_FORTRAN_COMPLEX ${OMPI_HAVE_FORTRAN_COMPLEX}

/* Whether we have Fortran 77 COMPLEX*16 or not */
#define OMPI_HAVE_FORTRAN_COMPLEX16 ${OMPI_HAVE_FORTRAN_COMPLEX16}

/* Whether we have Fortran 77 COMPLEX*32 or not */
#define OMPI_HAVE_FORTRAN_COMPLEX32 ${OMPI_HAVE_FORTRAN_COMPLEX32}

/* Whether we have Fortran 77 COMPLEX*8 or not */
#define OMPI_HAVE_FORTRAN_COMPLEX8 ${OMPI_HAVE_FORTRAN_COMPLEX8}

/* Whether we have Fortran 77 DOUBLE PRECISION or not */
#define OMPI_HAVE_FORTRAN_DOUBLE_PRECISION ${OMPI_HAVE_FORTRAN_DOUBLE_PRECISION}

/* Whether we have Fortran 77 INTEGER or not */
#define OMPI_HAVE_FORTRAN_INTEGER ${OMPI_HAVE_FORTRAN_INTEGER}

/* Whether we have Fortran 77 INTEGER*1 or not */
#define OMPI_HAVE_FORTRAN_INTEGER1 ${OMPI_HAVE_FORTRAN_INTEGER1}

/* Whether we have Fortran 77 INTEGER*16 or not */
#define OMPI_HAVE_FORTRAN_INTEGER16 ${OMPI_HAVE_FORTRAN_INTEGER16}

/* Whether we have Fortran 77 INTEGER*2 or not */
#define OMPI_HAVE_FORTRAN_INTEGER2 ${OMPI_HAVE_FORTRAN_INTEGER2}

/* Whether we have Fortran 77 INTEGER*4 or not */
#define OMPI_HAVE_FORTRAN_INTEGER4 ${OMPI_HAVE_FORTRAN_INTEGER4}

/* Whether we have Fortran 77 INTEGER*8 or not */
#define OMPI_HAVE_FORTRAN_INTEGER8 ${OMPI_HAVE_FORTRAN_INTEGER8}

/* Whether we have Fortran 77 LOGICAL or not */
#define OMPI_HAVE_FORTRAN_LOGICAL ${OMPI_HAVE_FORTRAN_LOGICAL}

/* Whether we have Fortran 77 LOGICAL1 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL1 ${OMPI_HAVE_FORTRAN_LOGICAL1}

/* Whether we have Fortran 77 LOGICAL2 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL2 ${OMPI_HAVE_FORTRAN_LOGICAL2}

/* Whether we have Fortran 77 LOGICAL4 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL4 ${OMPI_HAVE_FORTRAN_LOGICAL4}

/* Whether we have Fortran 77 LOGICAL8 or not */
#define OMPI_HAVE_FORTRAN_LOGICAL8 ${OMPI_HAVE_FORTRAN_LOGICAL8}

/* Whether we have Fortran 77 REAL or not */
#define OMPI_HAVE_FORTRAN_REAL ${OMPI_HAVE_FORTRAN_REAL}

/* Whether we have Fortran 77 REAL*16 or not */
#define OMPI_HAVE_FORTRAN_REAL16 ${OMPI_HAVE_FORTRAN_REAL16}

/* Whether we have Fortran 77 REAL*2 or not */
#define OMPI_HAVE_FORTRAN_REAL2 ${OMPI_HAVE_FORTRAN_REAL2}

/* Whether we have Fortran 77 REAL*4 or not */
#define OMPI_HAVE_FORTRAN_REAL4 ${OMPI_HAVE_FORTRAN_REAL4}

/* Whether we have Fortran 77 REAL*8 or not */
#define OMPI_HAVE_FORTRAN_REAL8 ${OMPI_HAVE_FORTRAN_REAL8}

#define OMPI_REAL16_MATCHES_C ${OMPI_REAL16_MATCHES_C}

/* Do not use outside of mpi.h. Define to 1 if the system has the type 'long
   long'. */
#cmakedefine OMPI_HAVE_LONG_LONG 1

/* Do we have POSIX threads */
#define OMPI_HAVE_POSIX_THREADS ${OMPI_HAVE_POSIX_THREADS}

/* If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK */
#cmakedefine OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK

/* If PTHREADS implementation supports PTHREAD_MUTEX_ERRORCHECK_NP */
#cmakedefine OMPI_HAVE_PTHREAD_MUTEX_ERRORCHECK_NP

/* Whether we have the sa_len struct in <sys/socket.h> or not */
#cmakedefine OMPI_HAVE_SA_LEN

/* Whether we have SA_RESTART in <signal.h> or not */
#cmakedefine OMPI_HAVE_SA_RESTART

/* Do we have native Solaris threads */
#define OMPI_HAVE_SOLARIS_THREADS ${OMPI_HAVE_SOLARIS_THREADS}

/* Whether we have __va_copy or not */
#cmakedefine OMPI_HAVE_UNDERSCORE_VA_COPY

/* Whether we have va_copy or not */
#cmakedefine OMPI_HAVE_VA_COPY

/* Wehther we have weak symbols or not */
#define OMPI_HAVE_WEAK_SYMBOLS ${OMPI_HAVE_WEAK_SYMBOLS}

/* Number of arguments to ibv_create_cq */
#cmakedefine OMPI_IBV_CREATE_CQ_ARGS

/* Major release number of Open MPI */
#cmakedefine OMPI_MAJOR_VERSION ${OMPI_MAJOR_VERSION_STRING}

/* Version of the GM API to use */
#cmakedefine OMPI_MCA_BTL_GM_API_VERSION

/* The GM build has or not a broker gm_get function */
#cmakedefine OMPI_MCA_BTL_GM_GET_BROKEN

/* Whether we have get_get() or not */
#cmakedefine OMPI_MCA_BTL_GM_HAVE_RDMA_GET

/* Whether we have gm_put() or gm_directed_send_with_callback() */
#cmakedefine OMPI_MCA_BTL_GM_HAVE_RDMA_PUT

/* The OS support or not the virtal page registration */
#cmakedefine OMPI_MCA_BTL_GM_SUPPORT_REGISTERING

/* Whether any opal memory mca components were found */
#cmakedefine OMPI_MEMORY_HAVE_COMPONENT

/* Trigger callbacks on sbrk instead of malloc or free */
#cmakedefine OMPI_MEMORY_PTMALLOC2_OPT_SBRK

/* Minor release number of Open MPI */
#cmakedefine OMPI_MINOR_VERSION ${OMPI_MINOR_VERSION_STRING}

/* Type of MPI_Offset -- has to be defined here and typedef'ed later because
   mpi.h does not get AC SUBST's */
#cmakedefine OMPI_MPI_OFFSET_TYPE ${OMPI_MPI_OFFSET_TYPE_STRING}

/* Version of the MX API to use */
#cmakedefine OMPI_MX_API_VERSION

/* Whether the C compiler supports "bool" without any other help (such as
   <stdbool.h>) */
#cmakedefine OMPI_NEED_C_BOOL 1

/* MPI datatype corresponding to MPI_Offset */
#cmakedefine OMPI_OFFSET_DATATYPE

/* Use the Cray XT-3 implementation of Portals */
#cmakedefine OMPI_PORTALS_CRAYXT3

/* Does Portals send a PTL_EVENT_UNLINK event */
#cmakedefine OMPI_PORTALS_HAVE_EVENT_UNLINK

/* Use the UTCP reference implementation of Portals */
#cmakedefine OMPI_PORTALS_UTCP

/* Whether r notation is used for ppc registers */
#cmakedefine OMPI_POWERPC_R_REGISTERS

/* Whether OMPI should provide MPI File interface */
#define OMPI_PROVIDE_MPI_FILE_INTERFACE ${OMPI_PROVIDE_MPI_FILE_INTERFACE}

/* type to use for ptrdiff_t */
#cmakedefine OMPI_PTRDIFF_TYPE ${OMPI_PTRDIFF_TYPE}

/* Release release number of Open MPI */
#cmakedefine OMPI_RELEASE_VERSION ${OMPI_RELEASE_VERSION_STRING}

/* whether to use cnos_barrier or not */
#cmakedefine OMPI_RML_CNOS_HAVE_BARRIER

/* Size of Fortran 77 COMPLEX */
#define OMPI_SIZEOF_FORTRAN_COMPLEX ${OMPI_SIZEOF_FORTRAN_COMPLEX}

/* Size of Fortran 77 COMPLEX*16 */
#define OMPI_SIZEOF_FORTRAN_COMPLEX16 ${OMPI_SIZEOF_FORTRAN_COMPLEX16}

/* Size of Fortran 77 COMPLEX*32 */
#define OMPI_SIZEOF_FORTRAN_COMPLEX32 ${OMPI_SIZEOF_FORTRAN_COMPLEX32}

/* Size of Fortran 77 COMPLEX*8 */
#define OMPI_SIZEOF_FORTRAN_COMPLEX8 ${OMPI_SIZEOF_FORTRAN_COMPLEX8}

/* Size of Fortran 77 DOUBLE PRECISION */
#define OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION ${OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION}

/* Size of Fortran 77 INTEGER */
#define OMPI_SIZEOF_FORTRAN_INTEGER ${OMPI_SIZEOF_FORTRAN_INTEGER}

/* Size of Fortran 77 INTEGER*1 */
#define OMPI_SIZEOF_FORTRAN_INTEGER1 ${OMPI_SIZEOF_FORTRAN_INTEGER1}

/* Size of Fortran 77 INTEGER*16 */
#define OMPI_SIZEOF_FORTRAN_INTEGER16 ${OMPI_SIZEOF_FORTRAN_INTEGER16}

/* Size of Fortran 77 INTEGER*2 */
#define OMPI_SIZEOF_FORTRAN_INTEGER2 ${OMPI_SIZEOF_FORTRAN_INTEGER2}

/* Size of Fortran 77 INTEGER*4 */
#define OMPI_SIZEOF_FORTRAN_INTEGER4 ${OMPI_SIZEOF_FORTRAN_INTEGER4}

/* Size of Fortran 77 INTEGER*8 */
#define OMPI_SIZEOF_FORTRAN_INTEGER8 ${OMPI_SIZEOF_FORTRAN_INTEGER8}

/* Size of Fortran 77 LOGICAL */
#define OMPI_SIZEOF_FORTRAN_LOGICAL ${OMPI_SIZEOF_FORTRAN_LOGICAL}

/* Size of Fortran 77 LOGICAL1 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL1 ${OMPI_SIZEOF_FORTRAN_LOGICAL1}

/* Size of Fortran 77 LOGICAL2 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL2 ${OMPI_SIZEOF_FORTRAN_LOGICAL2}

/* Size of Fortran 77 LOGICAL4 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL4 ${OMPI_SIZEOF_FORTRAN_LOGICAL4}

/* Size of Fortran 77 LOGICAL8 */
#define OMPI_SIZEOF_FORTRAN_LOGICAL8 ${OMPI_SIZEOF_FORTRAN_LOGICAL8}

/* Size of Fortran 77 REAL */
#define OMPI_SIZEOF_FORTRAN_REAL ${OMPI_SIZEOF_FORTRAN_REAL}

/* Size of Fortran 77 REAL*16 */
#define OMPI_SIZEOF_FORTRAN_REAL16 ${OMPI_SIZEOF_FORTRAN_REAL16}

/* Size of Fortran 77 REAL*2 */
#define OMPI_SIZEOF_FORTRAN_REAL2 ${OMPI_SIZEOF_FORTRAN_REAL2}

/* Size of Fortran 77 REAL*4 */
#define OMPI_SIZEOF_FORTRAN_REAL4 ${OMPI_SIZEOF_FORTRAN_REAL4}

/* Size of Fortran 77 REAL*8 */
#define OMPI_SIZEOF_FORTRAN_REAL8 ${OMPI_SIZEOF_FORTRAN_REAL8}

/* Do threads have different pids (pthreads on linux) */
#cmakedefine OMPI_THREADS_HAVE_DIFFERENT_PIDS

/* Whether to use <stdbool.h> or not */
#cmakedefine OMPI_USE_STDBOOL_H

/* Complete release number of Open MPI */
#cmakedefine OMPI_VERSION "${OMPI_VERSION_STRING}"

/* Whether we want MPI cxx support or not */
#define OMPI_WANT_CXX_BINDINGS ${OMPI_WANT_CXX_BINDINGS}

/* Whether we want the MPI f77 bindings or not */
#define OMPI_WANT_F77_BINDINGS ${OMPI_WANT_F77_BINDINGS}

/* Whether we want the MPI f90 bindings or not */
#define OMPI_WANT_F90_BINDINGS ${OMPI_WANT_F90_BINDINGS}

/* Whether to include support for libltdl or not */
#define OMPI_WANT_LIBLTDL ${OMPI_WANT_LIBLTDL}

/* do we want to try to work around C++ bindings SEEK_* issue? */
#cmakedefine OMPI_WANT_MPI_CXX_SEEK 1

/* if the peruse interface should be enabled */
#define OMPI_WANT_PERUSE ${OMPI_WANT_PERUSE}

/* if want pretty-print stack trace feature */
#cmakedefine OMPI_WANT_PRETTY_PRINT_STACKTRACE 1

/* whether we want to have smp locks in atomic ops or not */
#cmakedefine OMPI_WANT_SMP_LOCKS 1

/* Enable fault tolerance general components and logic */
#define OPAL_ENABLE_FT ${OPAL_ENABLE_FT}

/* Enable fault tolerance checkpoint/restart components and logic */
#cmakedefine OPAL_ENABLE_FT_CR 1

/* Enable fault tolerance thread in Open PAL */
#define OPAL_ENABLE_FT_THREAD ${OPAL_ENABLE_FT_THREAD}

/* Enable IPv6 support, but only if the underlying system supports it */
#cmakedefine OPAL_ENABLE_IPV6 1

/* Enable run-time tracing of internal functions */
#cmakedefine OPAL_ENABLE_TRACE 1

/* Greek - alpha, beta, etc - release number of Open Portable Access Layer */
#cmakedefine OPAL_GREEK_VERSION "${OPAL_GREEK_VERSION_STRING}"

/* Whether we have the _SC_NPROCESSORS_ONLN */
#cmakedefine OPAL_HAVE__SC_NPROCESSORS_ONLN

/* ident string for Open MPI */
#cmakedefine OPAL_IDENT_STRING "${OPAL_IDENT_STRING_VALUE}"

/* Major release number of Open Portable Access Layer */
#cmakedefine OPAL_MAJOR_VERSION ${OPAL_MAJOR_VERSION_STRING}

/* Minor release number of Open Portable Access Layer */
#cmakedefine OPAL_MINOR_VERSION ${OPAL_MINOR_VERSION_STRING}

/* package/branding string for Open MPI */
#cmakedefine OPAL_PACKAGE_STRING ${OPAL_PACKAGE_STRING}

/* Release release number of Open Portable Access Layer */
#cmakedefine OPAL_RELEASE_VERSION ${OPAL_RELEASE_VERSION_STRING}

/* Complete release number of Open Portable Access Layer */
#cmakedefine OPAL_VERSION "${OPAL_VERSION_STRING}"

/* Specific ps command to use in orte-clean */
#cmakedefine ORTE_CLEAN_PS_CMD

/* Enable full RTE support */
#define ORTE_DISABLE_FULL_SUPPORT ${ORTE_DISABLE_FULL_SUPPORT}

/* Greek - alpha, beta, etc - release number of Open Run-Time Environment */
#cmakedefine ORTE_GREEK_VERSION "${ORTE_GREEK_VERSION_STRING}"

/* Major release number of Open Run-Time Environment */
#cmakedefine ORTE_MAJOR_VERSION ${ORTE_MAJOR_VERSION_STRING}

/* Minor release number of Open Run-Time Environment */
#cmakedefine ORTE_MINOR_VERSION ${ORTE_MINOR_VERSION_STRING}

/* Release release number of Open Run-Time Environment */
#cmakedefine ORTE_RELEASE_VERSION ${ORTE_RELEASE_VERSION_STRING}

/* Complete release number of Open Run-Time Environment */
#cmakedefine ORTE_VERSION "${ORTE_VERSION_STRING}"

/* Whether we want orterun to effect "--prefix $prefix" by default */
#define ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT ${ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT}

/* Define to the address where bug reports for this package should be sent. */
#cmakedefine PACKAGE_BUGREPORT ${PACKAGE_BUGREPORT}

/* Define to the full name of this package. */
#cmakedefine PACKAGE_NAME "${PACKAGE_NAME}"

/* Define to the full name and version of this package. */
#cmakedefine PACKAGE_STRING "${PACKAGE_STRING}"

/* Define to the one symbol short name of this package. */
#cmakedefine PACKAGE_TARNAME "${PACKAGE_TARNAME}"

/* Define to the version of this package. */
#cmakedefine PACKAGE_VERSION "${PACKAGE_VERSION}"

/* The PLPA symbol prefix */
#cmakedefine PLPA_SYM_PREFIX

/* The PLPA symbol prefix in all caps */
#cmakedefine PLPA_SYM_PREFIX_CAPS

/* The size of `bool', as computed by sizeof. */
#cmakedefine SIZEOF_BOOL ${SIZEOF_BOOL}

/* The size of `char', as computed by sizeof. */
#cmakedefine SIZEOF_CHAR ${SIZEOF_CHAR}

/* The size of `double', as computed by sizeof. */
#cmakedefine SIZEOF_DOUBLE ${SIZEOF_DOUBLE}

/* The size of `float', as computed by sizeof. */
#cmakedefine SIZEOF_FLOAT ${SIZEOF_FLOAT}

/* The size of `int', as computed by sizeof. */
#cmakedefine SIZEOF_INT ${SIZEOF_INT}

/* The size of `long', as computed by sizeof. */
#cmakedefine SIZEOF_LONG ${SIZEOF_LONG}

/* The size of `long double', as computed by sizeof. */
#cmakedefine SIZEOF_LONG_DOUBLE ${SIZEOF_LONG_DOUBLE}

/* The size of `long long', as computed by sizeof. */
#cmakedefine SIZEOF_LONG_LONG ${SIZEOF_LONG_LONG}

/* The size of `pid_t', as computed by sizeof. */
#cmakedefine SIZEOF_PID_T ${SIZEOF_PID_T}

/* The size of `ptrdiff_t', as computed by sizeof. */
#cmakedefine SIZEOF_PTRDIFF_T ${SIZEOF_PTRDIFF_T}

/* The size of `short', as computed by sizeof. */
#cmakedefine SIZEOF_SHORT ${SIZEOF_SHORT}

/* The size of `size_t', as computed by sizeof. */
#cmakedefine SIZEOF_SIZE_T ${SIZEOF_SIZE_T}

/* The size of `ssize_t', as computed by sizeof. */
#cmakedefine SIZEOF_SSIZE_T ${SIZEOF_SSIZE_T}

/* The size of `void *', as computed by sizeof. */
#cmakedefine SIZEOF_VOID_P ${SIZEOF_VOID_P}

/* Define to 1 if you have the ANSI C header files. */
#cmakedefine STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#cmakedefine TIME_WITH_SYS_TIME 1

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
#cmakedefine WORDS_BIGENDIAN

/* Additional CFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_CFLAGS "${WRAPPER_EXTRA_CFLAGS}"

/* Additional CXXFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_CXXFLAGS "${WRAPPER_EXTRA_CXXFLAGS}"

/* Additional FCFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_FCFLAGS "${WRAPPER_EXTRA_FCFLAGS}"

/* Additional FFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_FFLAGS "${WRAPPER_EXTRA_FFLAGS}"

/* Additional LDFLAGS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_LDFLAGS "${WRAPPER_EXTRA_LDFLAGS}"

/* Additional LIBS to pass through the wrapper compilers */
#define WRAPPER_EXTRA_LIBS "${WRAPPER_EXTRA_LIBS}"

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#cmakedefine YYTEXT_POINTER

/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
#cmakedefine _GNU_SOURCE
#endif

/* Emulated value */
#cmakedefine __NR_sched_getaffinity

/* Emulated value */
#cmakedefine __NR_sched_setaffinity

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
#cmakedefine inline ${INLINE_STRING}
#endif

/* A bogus type that allows us to have sentinel type values that are still
   valid */
#cmakedefine ompi_fortran_bogus_type_t ${ompi_fortran_bogus_type_t}

/* C type corresponding to Fortran 77 COMPLEX*16 */
#cmakedefine ompi_fortran_complex16_t ${ompi_fortran_complex16_t}

/* C type corresponding to Fortran 77 COMPLEX*32 */
#cmakedefine ompi_fortran_complex32_t ${ompi_fortran_complex32_t}

/* C type corresponding to Fortran 77 COMPLEX*8 */
#cmakedefine ompi_fortran_complex8_t ${ompi_fortran_complex8_t}

/* C type corresponding to Fortran 77 COMPLEX */
#cmakedefine ompi_fortran_complex_t ${ompi_fortran_complex_t}

/* C type corresponding to Fortran 77 DOUBLE PRECISION */
#cmakedefine ompi_fortran_double_precision_t ${ompi_fortran_double_precision_t}

/* C type corresponding to Fortran 77 INTEGER*16 */
#cmakedefine ompi_fortran_integer16_t ${ompi_fortran_integer16_t}

/* C type corresponding to Fortran 77 INTEGER*1 */
#cmakedefine ompi_fortran_integer1_t ${ompi_fortran_integer1_t}

/* C type corresponding to Fortran 77 INTEGER*2 */
#cmakedefine ompi_fortran_integer2_t ${ompi_fortran_integer2_t}

/* C type corresponding to Fortran 77 INTEGER*4 */
#cmakedefine ompi_fortran_integer4_t ${ompi_fortran_integer4_t}

/* C type corresponding to Fortran 77 INTEGER*8 */
#cmakedefine ompi_fortran_integer8_t ${ompi_fortran_integer8_t}

/* C type corresponding to Fortran 77 INTEGER */
#cmakedefine ompi_fortran_integer_t ${ompi_fortran_integer_t}

/* C type corresponding to Fortran 77 LOGICAL */
#cmakedefine ompi_fortran_logical_t ${ompi_fortran_logical_t}

/* C type corresponding to Fortran 77 LOGICAL*1 */
#cmakedefine ompi_fortran_logical1_t ${ompi_fortran_logical1_t}

/* C type corresponding to Fortran 77 LOGICAL*2 */
#cmakedefine ompi_fortran_logical2_t ${ompi_fortran_logical2_t}

/* C type corresponding to Fortran 77 LOGICAL*4 */
#cmakedefine ompi_fortran_logical4_t ${ompi_fortran_logical4_t}

/* C type corresponding to Fortran 77 LOGICAL*8 */
#cmakedefine ompi_fortran_logical8_t ${ompi_fortran_logical8_t}

/* C type corresponding to Fortran 77 REAL*16 */
#cmakedefine ompi_fortran_real16_t ${ompi_fortran_real16_t}

/* C type corresponding to Fortran 77 REAL*2 */
#cmakedefine ompi_fortran_real2_t ${ompi_fortran_real2_t}

/* C type corresponding to Fortran 77 REAL*4 */
#cmakedefine ompi_fortran_real4_t ${ompi_fortran_real4_t}

/* C type corresponding to Fortran 77 REAL*8 */
#cmakedefine ompi_fortran_real8_t ${ompi_fortran_real8_t}

/* C type corresponding to Fortran 77 REAL */
#cmakedefine ompi_fortran_real_t ${ompi_fortran_real_t}

/* Define to `int' if <sys/types.h> does not define. */
#define pid_t ${PID_T}

/* Define to equivalent of C99 restrict keyword, or to nothing if this is not
   supported. Do not define if restrict is supported directly. */
#define restrict ${restrict}

/* Define to `unsigned int' if <sys/types.h> does not define. */
#cmakedefine size_t ${size_t}

/* Define to `unsigned short' if <sys/types.h> does not define. */
#cmakedefine u_int16_t ${u_int16_t}

/* Define to `unsigned int' if <sys/types.h> does not define. */
#cmakedefine u_int32_t ${u_int32_t}

/* Define to `unsigned long long' if <sys/types.h> does not define. */
#cmakedefine u_int64_t ${u_int64_t}

/* Define to `char' if <sys/types.h> does not define. */
#cmakedefine int8_t ${int8_t}

/* Define to `short' if <sys/types.h> does not define. */
#cmakedefine int16_t ${int16_t}

/* Define to `int' if <sys/types.h> does not define. */
#cmakedefine int32_t ${int32_t}

/* Define to `long long' if <sys/types.h> does not define. */
#cmakedefine int64_t ${int64_t}

/* Define to `unsigned char' if <sys/types.h> does not define. */
#cmakedefine u_int8_t ${u_int8_t}

#include "opal_config_bottom.h"
#endif /* OPAL_CONFIG_H */

