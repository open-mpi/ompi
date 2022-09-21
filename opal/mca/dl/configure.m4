dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl need in the core library
AC_DEFUN([MCA_opal_dl_CORE_LIB], [1])

dnl There will only be one component used in this framework, and it will
dnl be selected at configure time by priority.  Components must set
dnl their priorities in their configure.m4 file.

dnl We only want one winning component (vs. STOP_AT_FIRST_PRIORITY,
dnl which will allow all components of the same priority who succeed to
dnl win)
m4_define(MCA_opal_dl_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_opal_dl_CONFIG],[
    OPAL_HAVE_DL_SUPPORT=0

    # If --disable-dlopen was used, then have all the components fail
    # (we still need to configure them all so that things like "make
    # dist" work", but we just want the MCA system to (artificially)
    # conclude that it can't build any of the components.
    AS_IF([test "$enable_dlopen" = "no"],
          [want_dl=0], [want_dl=1])

    MCA_CONFIGURE_FRAMEWORK([opal], [dl], [$want_dl])

    # If we found no suitable static dl component and dlopen support
    # was not specifically disabled, this is an error.
    AS_IF([test "$MCA_opal_dl_STATIC_COMPONENTS" = "" && \
           test "$enable_dlopen" != "no"],
          [AC_MSG_WARN([Did not find a suitable static opal dl component])
           AC_MSG_WARN([You might need to install libltld (and its headers) or])
           AC_MSG_WARN([specify --disable-dlopen to configure.])
           AC_MSG_ERROR([Cannot continue])])

    # If we have a winning component (which, per above, will only
    # happen if --disable-dlopen was *not* specified), do some more
    # logic.
    AS_IF([test "$MCA_opal_dl_STATIC_COMPONENTS" != ""],
       [OPAL_HAVE_DL_SUPPORT=1])

    AC_DEFINE_UNQUOTED([OPAL_HAVE_DL_SUPPORT], [$OPAL_HAVE_DL_SUPPORT],
                       [Whether the OPAL DL framework is functional or not])
])
