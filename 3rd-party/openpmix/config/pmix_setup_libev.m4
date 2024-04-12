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

# PMIX_LIBEV_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
# Attempt to find a libev package.  If found, evaluate
# action-if-found.  Otherwise, evaluate action-if-not-found.
#
# Modifies the following in the environment:
#  * pmix_libev_CPPFLAGS
#  * pmix_libev_LDFLAGS
#  * pmix_libev_LIBS
#
# Adds the following to the wrapper compilers:
#  * CPPFLAGS: none
#  * LDLFGAS: add pmix_libev_LDFLAGS
#  * LIBS: add pmix_libev_LIBS
AC_DEFUN([PMIX_LIBEV_CONFIG],[
    PMIX_VAR_SCOPE_PUSH([pmix_event_dir pmix_event_libdir pmix_event_defaults pmix_check_libev_save_CPPFLAGS pmix_check_libev_save_LDFLAGS pmix_check_libev_save_LIBS])

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

    pmix_libev_support=1

    AS_IF([test "$with_libev" = "no"],
          [AC_MSG_NOTICE([Libev support disabled by user.])
           pmix_libev_support=0])

    AS_IF([test "$with_libev_extra_libs" = "yes" -o "$with_libev_extra_libs" = "no"],
	  [AC_MSG_ERROR([--with-libev-extra-libs requires an argument other than yes or no])])

    AS_IF([test $pmix_libev_support -eq 1],
          [pmix_check_libev_save_CPPFLAGS="$CPPFLAGS"
           pmix_check_libeve_save_LDFLAGS="$LDFLAGS"
           pmix_check_libev_save_LIBS="$LIBS"

           AS_IF([test "$enable_libev_lib_checks" != "no"],
                 [OAC_CHECK_PACKAGE([libev],
                                    [pmix_libev],
                                    [event.h],
                                    [ev ${with_libev_extra_libs}],
                                    [ev_async_send],
                                    [],
                                    [pmix_libev_support=0])],
                 [PMIX_FLAGS_APPEND_UNIQ([PMIX_DELAYED_LIBS], [$with_libev_extra_libs])])

           CPPFLAGS="$pmix_check_libev_save_CPPFLAGS"
           LDFLAGS="$pmix_check_libev_save_LDFLAGS"
           LIBS="$pmix_check_libev_save_LIBS"])

    AS_IF([test $pmix_libev_support -eq 1],
          [PMIX_FLAGS_APPEND_UNIQ([CPPFLAGS], [$pmix_libev_CPPFLAGS])
           PMIX_WRAPPER_FLAGS_ADD([CPPFLAGS], [$pmix_libev_CPPFLAGS])

           PMIX_FLAGS_APPEND_UNIQ([LDFLAGS], [$pmix_libev_LDFLAGS])
           PMIX_WRAPPER_FLAGS_ADD([LDFLAGS], [$pmix_libev_LDFLAGS])
           PMIX_WRAPPER_FLAGS_ADD([STATIC_LDFLAGS], [$pmix_libev_STATIC_LDFLAGS])

           PMIX_FLAGS_APPEND_UNIQ([PMIX_DELAYED_LIBS], [$pmix_libev_LIBS])
           PMIX_WRAPPER_FLAGS_ADD([LIBS], [$pmix_libev_LIBS])
           PMIX_WRAPPER_FLAGS_ADD([STATIC_LIBS], [$pmix_libev_STATIC_LIBS])

           PMIX_WRAPPER_FLAGS_ADD([PC_MODULES], [$pmix_hwloc_PC_MODULES])])

    AC_MSG_CHECKING([will libev support be built])
    if test $pmix_libev_support -eq 1; then
        AC_MSG_RESULT([yes])
        $1
        PMIX_SUMMARY_ADD([Required Packages], [Libev], [], [$pmix_libev_SUMMARY])
    else
        AC_MSG_RESULT([no])
        # if they asked us to use it, then this is an error
        AS_IF([test -n "$with_libev" && test "$with_libev" != "no"],
              [AC_MSG_WARN([LIBEV SUPPORT REQUESTED AND NOT FOUND])
               AC_MSG_ERROR([CANNOT CONTINUE])])
        $2
    fi

    PMIX_VAR_SCOPE_POP
])
