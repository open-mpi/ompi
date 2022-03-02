dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2021 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2021 Cisco Systems, Inc.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([_OPAL_CFLAGS_FAIL_SEARCH],[
    AC_REQUIRE([AC_PROG_GREP])
    if test -s conftest.err ; then
        $GREP -iq $1 conftest.err
        if test "$?" = "0" ; then
            opal_cv_cc_[$2]=0
        fi
    fi
])

AC_DEFUN([_OPAL_CHECK_SPECIFIC_CFLAGS], [
AC_MSG_CHECKING(if $CC supports ([$1]))
            CFLAGS_orig=$CFLAGS
            OPAL_FLAGS_APPEND_UNIQ([CFLAGS], ["$1"])
            AC_CACHE_VAL(opal_cv_cc_[$2], [
                   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [$3])],
                                   [
                                    opal_cv_cc_[$2]=1
                                    _OPAL_CFLAGS_FAIL_SEARCH(["ignored\|not recognized\|not supported\|not compatible\|unrecognized\|unknown"], [$2])
                                   ],
                                  [
                                    opal_cv_cc_[$2]=1
                                    _OPAL_CFLAGS_FAIL_SEARCH(["ignored\|not recognized\|not supported\|not compatible\|unrecognized\|unknown\|error"], [$2])
                                 ])])
            if test "$opal_cv_cc_[$2]" = "0" ; then
                CFLAGS="$CFLAGS_orig"
                AC_MSG_RESULT([no])
            else
                AC_MSG_RESULT([yes])
            fi
])

AC_DEFUN([_OPAL_CXXFLAGS_FAIL_SEARCH],[
    AC_REQUIRE([AC_PROG_GREP])
    if test -s conftest.err ; then
        $GREP -iq $1 conftest.err
        if test "$?" = "0" ; then
            opal_cv_cxx_[$2]=0
        fi
    fi
])

AC_DEFUN([_OPAL_CHECK_SPECIFIC_CXXFLAGS], [
AC_MSG_CHECKING(if $CXX supports ([$1]))
            CXXFLAGS_orig=$CXXFLAGS
            OPAL_FLAGS_APPEND_UNIQ([CXXFLAGS], ["$1"])
            AC_CACHE_VAL(opal_cv_cxx_[$2], [
                   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [$3])],
                                   [
                                    opal_cv_cxx_[$2]=1
                                    _OPAL_CXXFLAGS_FAIL_SEARCH(["ignored\|not recognized\|not supported\|not compatible\|unrecognized\|unknown"], [$2])
                                   ],
                                  [
                                    opal_cv_cxx_[$2]=1
                                    _OPAL_CXXFLAGS_FAIL_SEARCH(["ignored\|not recognized\|not supported\|not compatible\|unrecognized\|unknown\|error"], [$2])
                                 ])])
            if test "$opal_cv_cxx_[$2]" = "0" ; then
                CXXFLAGS="$CXXFLAGS_orig"
                AC_MSG_RESULT([no])
            else
                AC_MSG_RESULT([yes])
            fi
])
