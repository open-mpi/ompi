# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
#
# Copyright (c) 2017      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_pmix_pdl_plibltdl_PRIORITY], [50])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_pmix_pdl_plibltdl_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $1:$2 compile mode])
    $3="static"
    AC_MSG_RESULT([$$3])
])

# MCA_pmix_pdl_plibltdl_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_pmix_pdl_plibltdl_POST_CONFIG],[
    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"],
          [
           # Add some stuff to CPPFLAGS so that the rest of the source
           # tree can be built
           LDFLAGS="$LDFLAGS $pmix_pdl_plibltdl_ADD_LDFLAGS"
           LIBS="$LIBS $pmix_pdl_plibltdl_ADD_LIBS"
          ])
])dnl

# MCA_dl_plibltdl_CONFIG([action-if-can-compile],
#                       [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pdl_plibltdl_CONFIG],[
    PMIX_VAR_SCOPE_PUSH([CPPFLAGS_save LDFLAGS_save LIBS_save])
    AC_CONFIG_FILES([src/mca/pdl/plibltdl/Makefile])

    # Add --with options
    AC_ARG_WITH([plibltdl],
        [AC_HELP_STRING([--with-libltdl(=DIR)],
             [Build libltdl support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([libltdl-libdir],
       [AC_HELP_STRING([--with-libltdl-libdir=DIR],
             [Search for libltdl libraries in DIR])])

    # Sanity check the --with values
    PMIX_CHECK_WITHDIR([plibltdl], [$with_libltdl],
                       [include/ltdl.h])
    PMIX_CHECK_WITHDIR([plibltdl-libdir], [$with_libltdl_libdir],
                       [libltdl.*])

    # Defaults
    pmix_check_plibltdl_dir_msg="compiler default"
    pmix_check_plibltdl_libdir_msg="linker default"

    # Save directory names if supplied
    AS_IF([test ! -z "$with_libltdl" && test "$with_libltdl" != "yes"],
          [pmix_check_plibltdl_dir=$with_libltdl
           pmix_check_plibltdl_dir_msg="$pmix_check_plibltdl_dir (from --with-libltdl)"])
    AS_IF([test ! -z "$with_libltdl_libdir" && test "$with_libltdl_libdir" != "yes"],
          [pmix_check_plibltdl_libdir=$with_libltdl_libdir
           pmix_check_plibltdl_libdir_msg="$pmix_check_plibltdl_libdir (from --with-libltdl-libdir)"])

    pmix_pdl_plibltdl_happy=no
    AS_IF([test "$with_plibltdl" != "no"],
          [AC_MSG_CHECKING([for libltdl dir])
           AC_MSG_RESULT([$pmix_check_plibltdl_dir_msg])
           AC_MSG_CHECKING([for libltdl library dir])
           AC_MSG_RESULT([$pmix_check_plibltdl_libdir_msg])

           PMIX_CHECK_PACKAGE([pmix_pdl_plibltdl],
                  [ltdl.h],
                  [ltdl],
                  [lt_dlopen],
                  [],
                  [$pmix_check_plibltdl_dir],
                  [$pmix_check_plibltdl_libdir],
                  [pmix_pdl_plibltdl_happy=yes],
                  [pmix_pdl_plibltdl_happy=no])
              ])

    # If we have plibltdl, do we have lt_dladvise?
    pmix_pdl_plibltdl_have_lt_dladvise=0
    AS_IF([test "$pmix_pdl_plibltdl_happy" = "yes"],
          [CPPFLAGS_save=$CPPFLAGS
           LDFLAGS_save=$LDFLAGS
           LIBS_save=$LIBS

           CPPFLAGS="$pmix_pdl_plibltdl_CPPFLAGS $CPPFLAGS"
           LDFLAGS="$pmix_pdl_plibltdl_LDFLAGS $LDFLAGS"
           LIBS="$pmix_pdl_plibltdl_LIBS $LIBS"
           AC_CHECK_FUNC([lt_dladvise_init],
                         [pmix_pdl_plibltdl_have_lt_dladvise=1])
           CPPFLAGS=$CPPFLAGS_save
           LDFLAGS=$LDFLAGS_save
           LIBS=$LIBS_save
          ])
    AC_DEFINE_UNQUOTED(PMIX_PDL_PLIBLTDL_HAVE_LT_DLADVISE,
        [$pmix_pdl_plibltdl_have_lt_dladvise],
        [Whether we have lt_dladvise or not])

    AS_IF([test "$pmix_pdl_plibltdl_happy" = "yes"],
          [pmix_pdl_plibltdl_ADD_CPPFLAGS=$pmix_pdl_plibltdl_CPPFLAGS
           pmix_pdl_plibltdl_ADD_LDFLAGS=$pmix_pdl_plibltdl_LDFLAGS
           pmix_pdl_plibltdl_ADD_LIBS=$pmix_pdl_plibltdl_LIBS
           $1],
          [AS_IF([test ! -z "$with_libltdl" && \
                  test "$with_libltdl" != "no"],
                 [AC_MSG_WARN([libltdl support requested (via --with-libltdl) but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    AC_SUBST(pmix_pdl_plibltdl_CPPFLAGS)
    AC_SUBST(pmix_pdl_plibltdl_LDFLAGS)
    AC_SUBST(pmix_pdl_plibltdl_LIBS)

    PMIX_VAR_SCOPE_POP
])
