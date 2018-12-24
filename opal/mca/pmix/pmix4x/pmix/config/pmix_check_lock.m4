dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2017      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl Copyright (c) 2017      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2017      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2017      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_CHECK_DSTOR_LOCK],[
    orig_libs=$LIBS
    LIBS="-lpthread $LIBS"

    _x_ac_pthread_lock_found="0"
    _x_ac_fcntl_lock_found="0"

    AC_CHECK_MEMBERS([struct flock.l_type],
    [
        AC_DEFINE([HAVE_FCNTL_FLOCK], [1],
        [Define to 1 if you have the locking by fcntl.])
        _x_ac_fcntl_lock_found="1"
    ], [], [#include <fcntl.h>])

    if test "$DSTORE_PTHREAD_LOCK" = "1"; then
        AC_CHECK_FUNC([pthread_rwlockattr_setkind_np],
            [AC_EGREP_HEADER([PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP],
                    [pthread.h],[
                        AC_DEFINE([HAVE_PTHREAD_SETKIND], [1],
                            [Define to 1 if you have the `pthread_rwlockattr_setkind_np` function.])])])

        AC_CHECK_FUNC([pthread_rwlockattr_setpshared],
            [AC_EGREP_HEADER([PTHREAD_PROCESS_SHARED],
                    [pthread.h],[
                        AC_DEFINE([HAVE_PTHREAD_SHARED], [1],
                            [Define to 1 if you have the `PTHREAD_PROCESS_SHARED` definition.
                        ])
                        _x_ac_pthread_lock_found="1"
            ])
        ])

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
    AM_CONDITIONAL([HAVE_DSTORE_PTHREAD_LOCK], [test "$_x_ac_pthread_lock_found" = "1"])
    AM_CONDITIONAL([HAVE_DSTORE_FCNTL_LOCK], [test "$_x_ac_fcntl_lock_found" = "1"])
])
