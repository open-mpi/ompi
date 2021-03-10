dnl
dnl Definitions for using shared memory
dnl

dnl/*D
dnl PAC_ARG_SHARED_MEMORY - add --with-shared-memory=kind to configure
dnl
dnl Synopsis:
dnl PAC_ARG_SHARED_MEMORY
dnl
dnl Output effects:
dnl Adds '--with-shared-memory' to the command line. Checks for available
dnl shared memory functionality.
dnl
dnl Supported values of 'kind' include \:
dnl+    auto - default
dnl.    mmap - use mmap and munmap
dnl-    sysv - use sysv shared memory functions
dnl D*/
AC_DEFUN([PAC_ARG_SHARED_MEMORY],[

# check how to allocate shared memory
AC_ARG_WITH(shared-memory,
    AC_HELP_STRING([--with-shared-memory[=auto|sysv|mmap]], [create shared memory using sysv or mmap (default is auto)]),,
    with_shared_memory=auto)

if test "$with_shared_memory" = auto -o "$with_shared_memory" = mmap; then
    found_mmap_funcs=yes
    AC_CHECK_FUNCS(mmap munmap, , found_mmap_funcs=no)
    if test "$found_mmap_funcs" = yes ; then
        with_shared_memory=mmap
        AC_DEFINE(USE_MMAP_SHM,1,[Define if we have sysv shared memory])
        AC_MSG_NOTICE([Using a memory-mapped file for shared memory])
    elif test "$with_shared_memory" = mmap ; then
        AC_MSG_ERROR([cannot support shared memory:  mmap() or munmap() not found])
    fi
fi
if test "$with_shared_memory" = auto -o "$with_shared_memory" = sysv; then
    found_sysv_shm_funcs=yes
    AC_CHECK_FUNCS(shmget shmat shmctl shmdt, , found_sysv_shm_funcs=no)
    if test "$found_sysv_shm_funcs" = yes ; then
        with_shared_memory=sysv
        AC_DEFINE(USE_SYSV_SHM,1,[Define if we have sysv shared memory])
        AC_MSG_NOTICE([Using SYSV shared memory])
    elif test "$with_shared_memory" = sysv ; then
        AC_MSG_ERROR([cannot support shared memory:  sysv shared memory functions functions not found])
    fi
fi
])
