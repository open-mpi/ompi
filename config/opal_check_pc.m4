dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2021 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


dnl OPAL_CHECK_PKG_CONFIG()
dnl
dnl Check for availability of pkg-config and store the result.
dnl If it is not available, store any passed in libs from the
dnl --with-extra-libs configure option, or the known defaults.
dnl
dnl If it is available, allow configury to check for .pc files
dnl and append to OPAL_WRAPPER_EXTRA_LIBS.
AC_DEFUN([OPAL_CHECK_PKG_CONFIG], [
  AC_CHECK_PROG([PKG_CONFIG], [pkg-config], [pkg-config])
])

dnl OPAL_GET_LDFLAGS_FROM_PC(library name, variable-to-set-if-found,
dnl                          action-if-not-found)
AC_DEFUN([OPAL_GET_LDFLAGS_FROM_PC], [
  OPAL_VAR_SCOPE_PUSH([pkg_config_results happy])

  AC_REQUIRE([OPAL_CHECK_PKG_CONFIG])

  AC_MSG_CHECKING([for ldflags from pkg-config file $1])

  happy=1
  AS_IF([test "$PKG_CONFIG" = ""],
        [happy=0],
        [OPAL_LOG_COMMAND([pkg_config_results=`PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig $PKG_CONFIG --static --libs-only-L --libs-only-other $1`],
             [AS_VAR_COPY([$2], [pkg_config_results])],
             [happy=0])])
  AS_IF([test $happy -eq 0],
        [pkg_config_results="none"
         $3])

  AC_MSG_RESULT([$pkg_config_results])

  OPAL_VAR_SCOPE_POP
])

dnl OPAL_GET_LIBS_FROM_PC(library name, variable-to-set-if-found,
dnl                       action-if-not-found)
AC_DEFUN([OPAL_GET_LIBS_FROM_PC], [
  OPAL_VAR_SCOPE_PUSH([pkg_config_results happy])

  AC_REQUIRE([OPAL_CHECK_PKG_CONFIG])

  AC_MSG_CHECKING([for libs from pkg-config file $1])

  happy=1
  AS_IF([test "$PKG_CONFIG" = ""],
        [happy=0],
        [OPAL_LOG_COMMAND([pkg_config_results=`PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig $PKG_CONFIG --libs-only-l $1`],
             [AS_VAR_COPY([$2], [pkg_config_results])],
             [happy=0])])
  AS_IF([test $happy -eq 0],
        [pkg_config_results="none"
         $3])

  AC_MSG_RESULT([$pkg_config_results])

  OPAL_VAR_SCOPE_POP
])
