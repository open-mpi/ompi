# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


AC_DEFUN([_OMPI_CHECK_GM_CONFIG],[
    u_OMPI_CHECK_GM_CONFIG_SAVE_CPPFLAGS="$CPPFLAGS"
    u_OMPI_CHECK_GM_CONFIG_SAVE_LDFLAGS="$LDFLAGS"
    u_OMPI_CHECK_GM_CONFIG_SAVE_LIBS="$LIBS"

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
    LIBS="$LIBS $$1_LIBS"

    #
    # See if we have GM_API_VERSION.  If we do, use it.  If not, find the
    # highest one available.  It seems that GM_API_VERSION was introduced
    # somewhere after 1.3 but before 1.6. :-\
    #
    AC_MSG_CHECKING(for GM_API_VERSION)
    AC_TRY_COMPILE([#include<gm.h>],
    [int foo = GM_API_VERSION;], 
        have_gm_api_ver_msg=yes gm_api_ver=GM_API_VERSION,
        have_gm_api_ver_msg=no  gm_api_ver="")
    AC_MSG_RESULT([$have_gm_api_ver_msg])
    if test "$gm_api_ver" = ""; then
        found=0
        for val in 5 4 3; do
            if test "$found" = "0"; then
                var="GM_API_VERSION_1_$val"
                AC_MSG_CHECKING(for $var)
                AC_TRY_COMPILE([#include<gm.h>],
                    [int foo = $var;], 
                    msg=yes found=1 gm_api_ver=$var,
                    msg=no found=0 gm_api_ver="")
                AC_MSG_RESULT($msg)
            fi
        done
    fi
    if test "$gm_api_ver" = ""; then
        AC_MSG_WARN([*** Could not find a supported GM_API_VERSION])
        AC_MSG_ERROR([*** Cannot continue])
    fi
    AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_API_VERSION], $gm_api_ver,
        [Version of the GM API to use])
    unset gm_api_ver have_gm_api_ver_msg found val msg

    #
    # Do we have gm_put()?
    # gm_put() was introduced in gm 2.0, and is exactly identical to gm
    # 1.6's gm_directed_send_with_callback().  The name was simply changed
    # for consistency/symmtery with gm_get().
    #
    AC_MSG_CHECKING([for gm_put()])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include<gm.h>
    ]], 
    [[gm_put(0, 0, 0, 0, 0, 0, 0, 0, 0);]])], 
        [HAVE_RDMA_PUT=1 MSG=yes], 
        [HAVE_RDMA_PUT=0 MSG="no, use gm_directed_send_with_callback()"])
    AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_HAVE_RDMA_PUT], $HAVE_RDMA_PUT,
        [Whether we have gm_put() or gm_directed_send_with_callback()])
    AC_MSG_RESULT([$MSG])

    #
    # Do we have gm_get()?
    # gm_get() was introduced in gm 2.0.
    #
    AC_MSG_CHECKING([for gm_get()])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include<gm.h>
    ]], 
    [[gm_get(0, 0, 0, 0, 0, 0, 0, 0, 0);]])], 
        [HAVE_RDMA_GET=1 MSG=yes], 
        [HAVE_RDMA_GET=0 MSG=no])
    AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_HAVE_RDMA_GET], $HAVE_RDMA_GET,
        [Whether we have get_get() or not])
    AC_MSG_RESULT([$MSG])

    # Now test to see if the targetted GM is a broken one broken gm builds
    AC_MSG_CHECKING([for broken GM 2.x RDMA gets build])
    AC_TRY_COMPILE([
        #include <gm.h>
        ], [
        #if GM_API_VERSION_2_0 &&                                 \
           ((GM_API_VERSION_2_1_0 && GM_API_VERSION < 0x20102) || \
             GM_API_VERSION < 0x2000c)
          #error GM build is broken
        #endif         ],
       [ mca_gm_broken=0 gm_build_broken=no ],
       [ mca_gm_broken=1 gm_build_broken=yes ]) 
    AC_MSG_RESULT( [$gm_build_broken] )
    AC_DEFINE_UNQUOTED( [OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_GET_BROKEN], $mca_gm_broken,
                        [The GM build has or not a broker gm_get function] )
    unset gm_build_broken mca_gm_broken

    AC_DEFINE_UNQUOTED( [OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_SUPPORT_REGISTERING], 1,
                        [The OS support or not the virtal page registration] )

    CPPFLAGS="$u_OMPI_CHECK_GM_CONFIG_SAVE_CPPFLAGS"
    LDFLAGS="$u_OMPI_CHECK_GM_CONFIG_SAVE_LDFLAGS"
    LIBS="$u_OMPI_CHECK_GM_CONFIG_SAVE_LIBS"
])dnl

# OMPI_CHECK_GM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if GM support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_GM],[
    AC_ARG_WITH([gm],
        [AC_HELP_STRING([--with-gm(=DIR)],
             [Build GM (Myrinet) support, searching for libraries in DIR])])
    AC_ARG_WITH([gm-libdir],
        [AC_HELP_STRING([--with-gm-libdir=DIR],
             [Search for GM (Myrinet) libraries in DIR])])

    AS_IF([test "$with_gm" != "no"],
          [AS_IF([test ! -z "$with_gm" -a "$with_gm" != "yes"],
                 [ompi_check_gm_dir="$with_gm"])
           AS_IF([test ! -z "$with_gm_libdir" -a "$with_gm_libdir" != "yes"],
                 [ompi_check_gm_libdir="$with_gm_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [gm.h],
                              [gm],
                              [gm_init],
                              [],
                              [$ompi_check_gm_dir],
                              [$ompi_check_gm_libdir],
                              [ompi_check_gm_happy="yes"],
                              [ompi_check_gm_happy="no"])],
          [ompi_check_gm_happy="no"])

    AS_IF([test "$ompi_check_gm_happy" = "yes"],
          [_OMPI_CHECK_GM_CONFIG($1)
           $2],
          [AS_IF([test ! -z "$with_gm" -a "$with_gm" != "no"],
                 [AC_MSG_ERROR([GM support requested but not found.  Aborting])])
           $3])
])

