# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
#
# Copyright (c) 2017      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_dl_libltdl_PRIORITY], [50])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_dl_libltdl_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_dl_libltdl_CONFIG([action-if-can-compile],
#                       [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_dl_libltdl_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([CPPFLAGS_save LDFLAGS_save LIBS_save])
    AC_CONFIG_FILES([opal/mca/dl/libltdl/Makefile])

    # Add --with options
    AC_ARG_WITH([libltdl],
        [AS_HELP_STRING([--with-libltdl(=DIR)],
             [Build libltdl support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([libltdl-libdir],
       [AS_HELP_STRING([--with-libltdl-libdir=DIR],
             [Search for libltdl libraries in DIR])])

    OAC_CHECK_PACKAGE([libltdl],
                      [dl_libltdl],
                      [ltdl.h],
                      [ltdl],
                      [lt_dlopen],
                      [opal_dl_libltdl_happy=yes],
                      [opal_dl_libltdl_happy=no])

    # If we have libltdl, do we have lt_dladvise?
    opal_dl_libltdl_have_lt_dladvise=0
    AS_IF([test "$opal_dl_libltdl_happy" = "yes"],
          [CPPFLAGS_save=$CPPFLAGS
           LDFLAGS_save=$LDFLAGS
           LIBS_save=$LIBS

           CPPFLAGS="$dl_libltdl_CPPFLAGS $CPPFLAGS"
           LDFLAGS="$dl_libltdl_LDFLAGS $LDFLAGS"
           LIBS="$dl_libltdl_LIBS $LIBS"
           AC_CHECK_FUNC([lt_dladvise_init],
                         [opal_dl_libltdl_have_lt_dladvise=1])
           CPPFLAGS=$CPPFLAGS_save
           LDFLAGS=$LDFLAGS_save
           LIBS=$LIBS_save
          ])
    AC_DEFINE_UNQUOTED(OPAL_DL_LIBLTDL_HAVE_LT_DLADVISE,
        [$opal_dl_libltdl_have_lt_dladvise],
        [Whether we have lt_dladvise or not])

    AS_IF([test "$opal_dl_libltdl_happy" = "yes"],
          [$1],
          [AS_IF([test ! -z "$with_libltdl" && \
                  test "$with_libltdl" != "no"],
                 [AC_MSG_WARN([Libltdl support requested (via --with-libltdl) but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    AC_SUBST(dl_libltdl_CPPFLAGS)
    AC_SUBST(dl_libltdl_LDFLAGS)
    AC_SUBST(dl_libltdl_LIBS)

    OPAL_VAR_SCOPE_POP
])
