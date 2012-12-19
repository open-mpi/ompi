dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2010 Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


# OPAL_SETUP_WRAPPER_INIT()
# -------------------------
# Setup wrapper compiler configuration information - should be called
# before the bulk of the tests that can affect the wrapper compilers
#
# Note that we keep the user-specified flags seperately because we
# don't want them to go through OPAL_UNIQ because that has resulted in
# unexpected behavior for the user in the past.
AC_DEFUN([OPAL_SETUP_WRAPPER_INIT],[
    WRAPPER_EXTRA_CPPFLAGS=
    WRAPPER_EXTRA_CFLAGS=
    WRAPPER_EXTRA_CFLAGS_PREFIX=
    WRAPPER_EXTRA_CXXFLAGS=
    WRAPPER_EXTRA_CXXFLAGS_PREFIX=
    WRAPPER_EXTRA_LDFLAGS=
    WRAPPER_EXTRA_LIBS=

    USER_WRAPPER_EXTRA_CFLAGS=
    USER_WRAPPER_EXTRA_CFLAGS_PREFIX=
    USER_WRAPPER_EXTRA_CXXFLAGS=
    USER_WRAPPER_EXTRA_CXXFLAGS_PREFIX=
    USER_WRAPPER_EXTRA_LDFLAGS=
    USER_WRAPPER_EXTRA_LIBS=

    AC_ARG_WITH([wrapper-cflags], 
                [AC_HELP_STRING([--with-wrapper-cflags],
                                [Extra flags to add to CFLAGS when using mpicc])])
    if test "$with_wrapper_cflags" = "yes" -o "$with_wrapper_cflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cflags" ; then
        USER_WRAPPER_EXTRA_CFLAGS="$with_wrapper_cflags"
    fi

    AC_ARG_WITH([wrapper-cflags-prefix], 
                [AC_HELP_STRING([--with-wrapper-cflags-prefix],
                                [Extra flags to add to CFLAGS when using mpicc])])
    if test "$with_wrapper_cflags_prefix" = "yes" -o "$with_wrapper_cflags_prefix" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cflags-prefix must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cflags_prefix" ; then
        USER_WRAPPER_EXTRA_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
    fi

    AC_ARG_WITH([wrapper-cxxflags], 
        [AC_HELP_STRING([--with-wrapper-cxxflags],
                        [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
    if test "$with_wrapper_cxxflags" = "yes" -o "$with_wrapper_cxxflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cxxflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cxxflags" ; then
        USER_WRAPPER_EXTRA_CXXFLAGS="$with_wrapper_cxxflags"
    fi

    AC_ARG_WITH([wrapper-cxxflags-prefix], 
        [AC_HELP_STRING([--with-wrapper-cxxflags-prefix],
                        [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
    if test "$with_wrapper_cxxflags_prefix" = "yes" -o "$with_wrapper_cxxflags_prefix" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cxxflags-prefix must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cxxflags_prefix" ; then
        USER_WRAPPER_EXTRA_CXXFLAGS_PREFIX="$with_wrapper_cxxflags_prefix"
    fi

    AC_ARG_WITH([wrapper-ldflags], 
                [AC_HELP_STRING([--with-wrapper-ldflags],
                                [Extra flags to add to LDFLAGS when using wrapper compilers])])
    if test "$with_wrapper_ldflags" = "yes" -o "$with_wrapper_ldflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-ldflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_ldflags" ; then
        USER_WRAPPER_EXTRA_LDFLAGS="$with_wrapper_ldflags"
    fi

    AC_ARG_WITH([wrapper-libs], 
                [AC_HELP_STRING([--with-wrapper-libs],
                                [Extra flags to add to LIBS when using wrapper compilers])])
    if test "$with_wrapper_libs" = "yes" -o "$with_wrapper_libs" = "no"; then
        AC_MSG_ERROR([--with-wrapper-libs must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_libs" ; then
        USER_WRAPPER_EXTRA_LIBS="$with_wrapper_libs"
    fi
])


AC_DEFUN([OPAL_SETUP_WRAPPER_FINAL],[
    OPAL_UNIQ([WRAPPER_EXTRA_CPPFLAGS])
    OPAL_UNIQ([WRAPPER_EXTRA_CFLAGS])
    OPAL_UNIQ([WRAPPER_EXTRA_CFLAGS_PREFIX])
    OPAL_UNIQ([WRAPPER_EXTRA_CXXFLAGS])
    OPAL_UNIQ([WRAPPER_EXTRA_CXXFLAGS_PREFIX])
    OPAL_UNIQ([WRAPPER_EXTRA_LDFLAGS])

    OPAL_UNIQ([opal_WRAPPER_EXTRA_LDFLAGS])
    OPAL_UNIQ([opal_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for OPAL CPPFLAGS])
    OPAL_WRAPPER_EXTRA_CPPFLAGS="$WRAPPER_EXTRA_CPPFLAGS $USER_WRAPPER_EXTRA_CPPFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CPPFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CPPFLAGS])

    AC_MSG_CHECKING([for OPAL CFLAGS])
    OPAL_WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $USER_WRAPPER_EXTRA_CFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CFLAGS])

    AC_MSG_CHECKING([for OPAL CFLAGS_PREFIX])
    OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX="$WRAPPER_EXTRA_CFLAGS_PREFIX $USER_WRAPPER_EXTRA_CFLAGS_PREFIX"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX])

    AC_MSG_CHECKING([for OPAL CXXFLAGS])
    OPAL_WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS $USER_WRAPPER_EXTRA_CXXFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CXXFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CXXFLAGS])

    AC_MSG_CHECKING([for OPAL CXXFLAGS_PREFIX])
    OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX="$WRAPPER_EXTRA_CXXFLAGS_PREFIX $USER_WRAPPER_EXTRA_CXXFLAGS_PREFIX"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX])

    AC_MSG_CHECKING([for OPAL LDFLAGS])
    OPAL_WRAPPER_EXTRA_LDFLAGS="$opal_WRAPPER_EXTRA_LDFLAGS $WRAPPER_EXTRA_LDFLAGS $USER_WRAPPER_EXTRA_LDFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_LDFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_LDFLAGS])

    AC_MSG_CHECKING([for OPAL LIBS])
    OPAL_WRAPPER_EXTRA_LIBS="$opal_WRAPPER_EXTRA_LIBS $WRAPPER_EXTRA_LIBS $USER_WRAPPER_EXTRA_LIBS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_LIBS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for OPAL extra include dirs])
    if test "$WANT_INSTALL_HEADERS" = "1" ; then
        OPAL_WRAPPER_EXTRA_INCLUDES="openmpi"
    else
        OPAL_WRAPPER_EXTRA_INCLUDES=
    fi
    AC_SUBST([OPAL_WRAPPER_EXTRA_INCLUDES])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_INCLUDES])


    # For script-based wrappers that don't do relocatable binaries.
    # Don't use if you don't have to.
    exec_prefix_save="${exec_prefix}"
    test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"
    eval "OPAL_WRAPPER_INCLUDEDIR=\"${includedir}\""
    eval "OPAL_WRAPPER_LIBDIR=\"${libdir}\""
    exec_prefix="${exec_prefix_save}"
    AC_SUBST([OPAL_WRAPPER_INCLUDEDIR])
    AC_SUBST([OPAL_WRAPPER_LIBDIR])

    # compatibility defines that will eventually go away
    WRAPPER_EXTRA_CFLAGS="$OPAL_WRAPPER_EXTRA_CFLAGS"
    WRAPPER_EXTRA_CFLAGS_PREFIX="$OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX"
    WRAPPER_EXTRA_CXXFLAGS="$OPAL_WRAPPER_EXTRA_CXXFLAGS"
    WRAPPER_EXTRA_CXXFLAGS_PREFIX="$OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX"
    WRAPPER_EXTRA_LDFLAGS="$OPAL_WRAPPER_EXTRA_LDFLAGS"
    WRAPPER_EXTRA_LIBS="$OPAL_WRAPPER_EXTRA_LIBS"

    AC_SUBST([WRAPPER_EXTRA_CFLAGS])
    AC_SUBST([WRAPPER_EXTRA_CFLAGS_PREFIX])
    AC_SUBST([WRAPPER_EXTRA_CXXFLAGS])
    AC_SUBST([WRAPPER_EXTRA_CXXFLAGS_PREFIX])
    AC_SUBST([WRAPPER_EXTRA_LDFLAGS])
    AC_SUBST([WRAPPER_EXTRA_LIBS])

    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$WRAPPER_EXTRA_CFLAGS",
        [Additional CFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS_PREFIX, "$WRAPPER_EXTRA_CFLAGS_PREFIX",
        [Additional CFLAGS_PREFIX to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS, "$WRAPPER_EXTRA_CXXFLAGS",
        [Additional CXXFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS_PREFIX, "$WRAPPER_EXTRA_CXXFLAGS_PREFIX",
        [Additional CXXFLAGS_PREFIX to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LDFLAGS, "$WRAPPER_EXTRA_LDFLAGS",
        [Additional LDFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LIBS, "$WRAPPER_EXTRA_LIBS",
        [Additional LIBS to pass through the wrapper compilers])
])
