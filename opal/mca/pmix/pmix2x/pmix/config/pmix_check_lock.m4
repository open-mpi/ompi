dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2017      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl Copyright (c) 2017      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2017      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_CHECK_DSTOR_LOCK],[

    PMIX_VAR_SCOPE_PUSH(orig_libs pmix_prefer_write_nonrecursive)

    orig_libs=$LIBS
    LIBS="-lpthread $LIBS"

    _x_ac_pthread_lock_found=0
    _x_ac_fcntl_lock_found=0
    pmix_prefer_write_nonrecursive=0

    AC_CHECK_MEMBER([struct flock.l_type],
                    [pmix_fcntl_flock_happy=yes
                     _x_ac_fcntl_lock_found=1],
                    [pmix_fcntl_flock_happy=no],
                    [#include <fcntl.h>])

    if test "$DSTORE_PTHREAD_LOCK" = "1"; then

        AC_MSG_CHECKING([pthread_process_shared])
        AC_EGREP_CPP([yes],
                     [#include <pthread.h>
                      #ifdef PTHREAD_PROCESS_SHARED
                        yes
                      #endif
                     ],
                     [AC_MSG_RESULT(yes)
                      pmix_pthread_process_shared=yes],
                     [AC_MSG_RESULT(no)
                      pmix_pthread_process_shared=no])

        AC_CHECK_FUNC([pthread_rwlockattr_setkind_np],
                      [pmix_pthread_rwlockattr_setkind_np=yes
                       AC_EGREP_CPP([yes],
                                    [#include <pthread.h>
                                     #ifdef PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP
                                       yes
                                     #endif
                                    ],
                                    [pmix_pthread_rwlock_prefer_writer_nonrecursive_np=yes],
                                    [pmix_pthread_rwlock_prefer_writer_nonrecursive_np=no])],
            [pmix_pthread_rwlockattr_setkind_np=no])

        AC_CHECK_FUNC([pthread_rwlockattr_setpshared],
                      [pmix_pthread_rwlockattr_setpshared=yes
                       AS_IF([test "$pmix_pthread_process_shared" = "yes"],
                            [_x_ac_pthread_lock_found=1]]),
                      [pmix_pthread_rwlockattr_setpshared=no])

        AC_CHECK_FUNC([pthread_mutexattr_setpshared],
                      [pmix_pthread_mutexattr_setpshared=yes],
                      [pmix_pthread_mutexattr_setpshared=no])

        AS_IF([test "$pmix_pthread_rwlockattr_setkind_np" = "yes" && test "$pmix_pthread_rwlock_prefer_writer_nonrecursive_np" = "yes"],
              [pmix_prefer_write_nonrecursive=1],
              [pmix_prefer_write_nonrecursive=0])

        if test "$_x_ac_pthread_lock_found" = "0"; then
            if test "$_x_ac_fcntl_lock_found" = "1"; then
                AC_MSG_WARN([dstore: pthread-based locking not found, will use fcntl-based locking.])
            else
                AC_MSG_ERROR([dstore: no available locking mechanisms was found. Can not continue. Try disabling dstore])
            fi
        fi
    else
        if test "$_x_ac_fcntl_lock_found" = "0"; then
            AC_MSG_ERROR([dstore: no available locking mechanisms was found. Can not continue. Try disabling dstore])
        fi
    fi
    LIBS="$orig_libs"

    AC_DEFINE_UNQUOTED([PMIX_PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP],
                       [$pmix_prefer_write_nonrecursive],
                       [Whether or not we found the optional write_nonrecursive_np flag])
    AM_CONDITIONAL([HAVE_DSTORE_PTHREAD_LOCK], [test "$_x_ac_pthread_lock_found" = "1"])
    AM_CONDITIONAL([HAVE_DSTORE_FCNTL_LOCK], [test "$_x_ac_fcntl_lock_found" = "1"])

    PMIX_VAR_SCOPE_POP
])
