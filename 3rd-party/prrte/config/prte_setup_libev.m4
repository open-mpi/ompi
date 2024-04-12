# -*- shell-script -*-
#
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# PRTE_LIBEV_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
# Attempt to find a libev package.  If found, evaluate
# action-if-found.  Otherwise, evaluate action-if-not-found.
#
# Modifies the following in the environment:
#  * prte_libev_CPPFLAGS
#  * prte_libev_LDFLAGS
#  * prte_libev_LIBS
#
# Adds the following to the wrapper compilers:
#  * CPPFLAGS: none
#  * LDLFGAS: add prte_libev_LDFLAGS
#  * LIBS: add prte_libev_LIBS
AC_DEFUN([PRTE_LIBEV_CONFIG],[
    PRTE_VAR_SCOPE_PUSH([prte_event_dir prte_event_libdir prte_event_defaults prte_check_libev_save_CPPFLAGS prte_check_libev_save_LDFLAGS prte_check_libev_save_LIBS])

    AC_ARG_WITH([libev],
                [AS_HELP_STRING([--with-libev=DIR],
                                [Search for libev headers and libraries in DIR ])])
    AC_ARG_WITH([libev-libdir],
                [AS_HELP_STRING([--with-libev-libdir=DIR],
                                [Search for libev libraries in DIR ])])
    AC_ARG_WITH([libev-extra-libs],
                [AS_HELP_STRING([--with-libev-extra-libs=LIBS],
                                [Add LIBS as dependencies of Libev])])
    AC_ARG_ENABLE([libev-lib-checks],
                   [AS_HELP_STRING([--disable-libev-lib-checks],
                                   [If --disable-libev-lib-checks is specified, configure will assume that -lev is available])])

    prte_libev_support=1

    AS_IF([test "$with_libev" = "no"],
          [AC_MSG_NOTICE([Libev support disabled by user.])
           prte_libev_support=0])

    AS_IF([test "$with_libev_extra_libs" = "yes" -o "$with_libev_extra_libs" = "no"],
	  [AC_MSG_ERROR([--with-libev-extra-libs requires an argument other than yes or no])])

    AS_IF([test $prte_libev_support -eq 1],
          [prte_check_libev_save_CPPFLAGS="$CPPFLAGS"
           prte_check_libeve_save_LDFLAGS="$LDFLAGS"
           prte_check_libev_save_LIBS="$LIBS"

           AS_IF([test "$enable_libev_lib_checks" != "no"],
                 [OAC_CHECK_PACKAGE([libev],
                                    [prte_libev],
                                    [event.h],
                                    [ev ${with_libev_extra_libs}],
                                    [ev_async_send],
                                    [],
                                    [prte_libev_support=0])],
                 [PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LIBS], [$with_libev_extra_libs])])

           CPPFLAGS="$prte_check_libev_save_CPPFLAGS"
           LDFLAGS="$prte_check_libev_save_LDFLAGS"
           LIBS="$prte_check_libev_save_LIBS"])

    AS_IF([test $prte_libev_support -eq 1],
          [PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_CPPFLAGS], [$prte_libev_CPPFLAGS])

           PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LDFLAGS], [$prte_libev_LDFLAGS])

           PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LIBS], [$prte_libev_LIBS])])

    AC_MSG_CHECKING([will libev support be built])
    if test $prte_libev_support -eq 1; then
        AC_MSG_RESULT([yes])
        $1
        PRTE_SUMMARY_ADD([Required Packages], [Libev], [], [$prte_libev_SUMMARY])
    else
        AC_MSG_RESULT([no])
        # if they asked us to use it, then this is an error
        AS_IF([test -n "$with_libev" && test "$with_libev" != "no"],
              [AC_MSG_WARN([LIBEV SUPPORT REQUESTED AND NOT FOUND])
               AC_MSG_ERROR([CANNOT CONTINUE])])
        $2
    fi

    PRTE_VAR_SCOPE_POP
])
