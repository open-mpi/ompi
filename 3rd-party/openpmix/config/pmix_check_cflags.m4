dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2021 IBM Corporation.  All rights reserved.
dnl
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([_PMIX_CFLAGS_FAIL_SEARCH],[
    AC_REQUIRE([AC_PROG_GREP])
    if test -s conftest.err ; then
        $GREP -iq $1 conftest.err
        if test "$?" = "0" ; then
            pmix_cv_cc_[$2]=0
        fi
    fi
])

AC_DEFUN([_PMIX_CHECK_SPECIFIC_CFLAGS], [
AC_MSG_CHECKING(if $CC supports ([$1]))
            CFLAGS_orig=$CFLAGS
            PMIX_APPEND_UNIQ([CFLAGS], ["$1"])
            AC_CACHE_VAL(pmix_cv_cc_[$2], [
                   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [$3])],
                                   [
                                    pmix_cv_cc_[$2]=1
                                    _PMIX_CFLAGS_FAIL_SEARCH("ignored\|not recognized\|not supported\|not compatible\|unrecognized\|unknown", [$2])
                                   ],
                                    pmix_cv_cc_[$2]=1
                                    _PMIX_CFLAGS_FAIL_SEARCH("ignored\|not recognized\|not supported\|not compatible\|unrecognized\|unknown", [$2])
                                 )])
            if test "$pmix_cv_cc_[$2]" = "0" ; then
                CFLAGS="$CFLAGS_orig"
                AC_MSG_RESULT([no])
            else
                AC_MSG_RESULT([yes])
            fi
])
