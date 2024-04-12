# -*- shell-script -*-
#
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
#
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_prte_prtedl_libltdl_PRIORITY], [50])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_prte_prtedl_libltdl_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $1:$2 compile mode])
    $3="static"
    AC_MSG_RESULT([$$3])
])

# MCA_prte_prtedl_libltdl_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_prte_prtedl_libltdl_POST_CONFIG],[
    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"],
          [
           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           LDFLAGS="$LDFLAGS $prte_prtedl_libltdl_ADD_LDFLAGS"
           LIBS="$LIBS $prte_prtedl_libltdl_ADD_LIBS"
          ])
])dnl

# MCA_prtedl_libltdl_CONFIG([action-if-can-compile],
#                       [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_prte_prtedl_libltdl_CONFIG],[
    PRTE_VAR_SCOPE_PUSH([CPPFLAGS_save LDFLAGS_save LIBS_save])
    AC_CONFIG_FILES([src/mca/prtedl/libltdl/Makefile])

    # Add --with options
    AC_ARG_WITH([libltdl],
        [AS_HELP_STRING([--with-libltdl(=DIR)],
             [Build libltdl support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([libltdl-libdir],
       [AS_HELP_STRING([--with-libltdl-libdir=DIR],
             [Search for libltdl libraries in DIR])])

    prte_prtedl_libltdl_happy=no
    AS_IF([test "$with_libltdl" != "no"],
          [OAC_CHECK_PACKAGE([libltdl],
                  [prte_prtedl_libltdl],
                  [ltprtedl.h],
                  [ltprtedl],
                  [lt_dlopen],
                  [prte_prtedl_libltdl_happy=yes],
                  [prte_prtedl_libltdl_happy=no])
              ])

    # If we have libltdl, do we have lt_dladvise?
    prte_prtedl_libltdl_have_lt_dladvise=0
    AS_IF([test "$prte_prtedl_libltdl_happy" = "yes"],
          [CPPFLAGS_save=$CPPFLAGS
           LDFLAGS_save=$LDFLAGS
           LIBS_save=$LIBS

           CPPFLAGS="$prte_prtedl_libltdl_CPPFLAGS $CPPFLAGS"
           LDFLAGS="$prte_prtedl_libltdl_LDFLAGS $LDFLAGS"
           LIBS="$prte_prtedl_libltdl_LIBS $LIBS"
           AC_CHECK_FUNC([lt_dladvise_init],
                         [prte_prtedl_libltdl_have_lt_dladvise=1])
           CPPFLAGS=$CPPFLAGS_save
           LDFLAGS=$LDFLAGS_save
           LIBS=$LIBS_save
          ])
    AC_DEFINE_UNQUOTED(PRTE_DL_LIBLTDL_HAVE_LT_DLADVISE,
        [$prte_prtedl_libltdl_have_lt_dladvise],
        [Whether we have lt_dladvise or not])

    AS_IF([test "$prte_prtedl_libltdl_happy" = "yes"],
          [prte_prtedl_libltdl_ADD_CPPFLAGS=$prte_prtedl_libltdl_CPPFLAGS
           prte_prtedl_libltdl_ADD_LDFLAGS=$prte_prtedl_libltdl_LDFLAGS
           prte_prtedl_libltdl_ADD_LIBS=$prte_prtedl_libltdl_LIBS
           $1],
          [AS_IF([test ! -z "$with_libltdl" && \
                  test "$with_libltdl" != "no"],
                 [AC_MSG_WARN([Libltdl support requested (via --with-libltdl) but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    AC_SUBST(prte_prtedl_libltdl_CPPFLAGS)
    AC_SUBST(prte_prtedl_libltdl_LDFLAGS)
    AC_SUBST(prte_prtedl_libltdl_LIBS)

    PRTE_VAR_SCOPE_POP
])
