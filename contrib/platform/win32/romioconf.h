/* adio/include/romioconf.h.  Generated from romioconf.h.in by configure.  */
/* adio/include/romioconf.h.in.  Generated from configure.in by autoheader.  */

#include "romioconf-undefs.h"

/* Define if no signotify */
/* #undef AIO_SIGNOTIFY_NONE */

/* Define for AIX */
/* #undef AIX */

/* Define if Cray */
/* #undef CRAY */

/* Define if aio.h exists and can be compiled */
/*#define HAVE_AIO_H 0*/

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fsync' function. */
#define HAVE_FSYNC 1

/* Define to 1 if you have the `ftruncate' function. */
#define HAVE_FTRUNCATE 1

/* Define if GNU __attribute__ is supported */
/* #define HAVE_GCC_ATTRIBUTE 1 */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define if long long is 64 bits */
#define HAVE_LONG_LONG_64 1

/* Define to 1 if you have the `lstat' function. */
#define HAVE_LSTAT 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `memalign' function. */
#define HAVE_MEMALIGN 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define if MPI combiners available */
/* #undef HAVE_MPI_COMBINERS */

/* Define if Darray is available */
#define HAVE_MPI_DARRAY_SUBARRAY 1

/* Define if generalized requests avaliable */
/* #undef HAVE_MPI_GREQUEST */

/* Define if MPI Info is available */
#define HAVE_MPI_INFO 1

/* Define if supports long long int */
/* #undef HAVE_MPI_LONG_LONG_INT */

/* Will always be 1 - OMPI has MPI_OFFSET */
#define HAVE_MPI_OFFSET 1

/* Define for CRAY weak dup */
/* #undef HAVE_PRAGMA_CRI_DUP */

/* Define for HP weak pragma */
/* #undef HAVE_PRAGMA_HP_SEC_DEF */

/* Define if pragma weak available */
/* #undef HAVE_PRAGMA_WEAK */

/* Define if pread64 available */
/* #undef HAVE_PREAD64 */

/* Define to 1 if you have the <pvfs2.h> header file. */
/* #undef HAVE_PVFS2_H */

/* Define if PVFS2_SUPER_MAGIC defined. */
/* #undef HAVE_PVFS2_SUPER_MAGIC */

/* Define to 1 if you have the <pvfs.h> header file. */
/* #undef HAVE_PVFS_H */

/* Define if PVFS_SUPER_MAGIC defined. */
/* #undef HAVE_PVFS_SUPER_MAGIC */

/* Define to 1 if you have the `readlink' function. */
#define HAVE_READLINK 1

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if you have the `snprintf' function. */
/*#define HAVE_SNPRINTF 0*/

/* Define if stat function is present */
#define HAVE_STAT 1

/* Define if status set bytes available */
#define HAVE_STATUS_SET_BYTES 1

/* Define if statvfs function is present */
#define HAVE_STATVFS 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define if struct statfs can be compiled */
#define HAVE_STRUCT_STATFS 1

/* Define if sys/aio.h exists and can be compiled */
/* #undef HAVE_SYS_AIO_H */

/* Define to 1 if you have the <sys/mount.h> header file. */
#define HAVE_SYS_MOUNT_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
/*#define HAVE_SYS_PARAM_H 1*/

/* Define to 1 if you have the <sys/statvfs.h> header file. */
#define HAVE_SYS_STATVFS_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/vfs.h> header file. */
#define HAVE_SYS_VFS_H 1

/* Define to 1 if you have the <unistd.h> header file. */
/*#define HAVE_UNISTD_H 1*/

/* Define if weak symbols available */
/* #undef HAVE_WEAK_SYMBOLS */

/* Define for HPUX */
/* #undef HPUX */

/* Define if int smaller than pointer */
/*#define INT_LT_POINTER 1*/

/* Define if IRIX */
/* #undef IRIX */

/* Define if compiling within MPICH */
/* #undef MPICH */

/* Define if using HP MPI */
/* #undef MPIHP */

/* Define if compiling within LAM/MPI */
/* #undef MPILAM */

/* hack to make ROMIO build without profiling */
#define MPIO_BUILD_PROFILING 1

/* Define if SGI MPI */
/* #undef MPISGI */

/* Define if MPI_Offset is int */
/* #undef MPI_OFFSET_IS_INT */

/* Define if fsync needs a declaration */
/* #undef NEEDS_FSYNC_DECL */

/* Define if ftruncate needs a declaration */
/* #undef NEEDS_FTRUNCATE_DECL */

/* Define if l_start and l_len data should be cast as int */
/* #undef NEEDS_INT_CAST_WITH_FLOCK */

/* Define if lstat needs a declaration */
#define NEEDS_LSTAT_DECL 1

/* Define if mpi_test needed */
/* #undef NEEDS_MPI_TEST */

/* Define if readlink needs a declaration */
/* #undef NEEDS_READLINK_DECL */

/* Define if snprintf needs a declaration */
/* #undef NEEDS_SNPRINTF_DECL */

/* Define if strdup needs a declaration */
/* #undef NEEDS_STRDUP_DECL */

/* Define if no MPI type is contig */
/* #undef NO_MPI_SGI_type_is_contig */

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#define PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME ""

/* Define to the version of this package. */
#define PACKAGE_VERSION ""

/* Define for printing error messages */
/* #undef PRINT_ERR_MSG */

/* Define for ROMIO with gridftp */
/* #undef ROMIO_GRIDFTP */

/* Define if AIO calls need file descriptor */
/* #undef ROMIO_HAVE_AIO_CALLS_NEED_FILEDES */

/* Define if aio_suspend needs two arguments */
/* #undef ROMIO_HAVE_AIO_SUSPEND_TWO_ARGS */

/* Define if aiocb has aio_fildes member */
#define ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_FILDES 1

/* Define if aiocb has aio_handle member */
/* #undef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_HANDLE */

/* Define if aiocb has aio_reqprio member */
#define ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_REQPRIO 1

/* Define if aiocb has aio_sigevent member */
#define ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_SIGEVENT 1

/* Define if aiocb has aio_whence member */
/* #undef ROMIO_HAVE_STRUCT_AIOCB_WITH_AIO_WHENCE */

/* Define if statfs has f_fstypename */
/* #undef ROMIO_HAVE_STRUCT_STATFS_WITH_F_FSTYPENAME */

/* defined if struct statvfs has a f_basetype member */
/* #undef ROMIO_HAVE_STRUCT_STATVFS_WITH_F_BASETYPE */

/* Define if struct stat has a st_fstype member */
/* #undef ROMIO_HAVE_STRUCT_STAT_WITH_ST_FSTYPE */

/* Define if AIO calls seem to work */
#define ROMIO_HAVE_WORKING_AIO 1

/* Define for ROMIO with HFS */
/* #undef ROMIO_HFS */

/* Define if compiling within MPICH2 */
/* #undef ROMIO_INSIDE_MPICH2 */

/* Define for ROMIO with NFS */
#define ROMIO_NFS 1

/* Define for ROMIO with NTFS */
#define ROMIO_NTFS 1

/* Define for ROMIO with PANFS */
/* #undef ROMIO_PANFS */

/* Define for ROMIO with PFS */
/* #undef ROMIO_PFS */

/* Define for ROMIO with PVFS */
/* #undef ROMIO_PVFS */

/* Define for ROMIO with PVFS2 */
/* #undef ROMIO_PVFS2 */

/* Define if int64_t must be defined for PVFS */
/* #undef ROMIO_PVFS_NEEDS_INT64_DEFINITION */

/* Define for ROMIO with SFS */
/* #undef ROMIO_SFS */

/* Define for ROMIO with TESTFS */
#define ROMIO_TESTFS 1

/* Define for ROMIO with UFS */
#define ROMIO_UFS 1

/* Define for ROMIO with XFS */
/* #undef ROMIO_XFS */

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* Define for SPPUX (Convex) */
/* #undef SPPUX */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define for NEC SX4 */
/* #undef SX4 */

/* if C does not support restrict */
 #define restrict
