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
dnl Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_SETUP_COMPONENT_PACKAGE(1: framework_name, 
#                              2: component_name,
#                              3: option_name,
#                              4: withdir_dir_check_file,
#                              5: withdir_libdir_check_file,
#                              6: header, 
#                              7: library, 
#                              8: function, 
#                              9: extra-libraries, 
#                              10: [action-if-found], 
#                              11: [action-if-not-found])
# ------------------------------------------------
# Many components need to just check for one package, and if it's all
# good, set themselves up with appropriate CPPFLAGS, LDFLAGS, and
# LIBS.  This macro templates all of that for the common case.
#
# This macro does the following:
#
# - Assumes that this component should be built by default if all
#   headers and libraries can be found
# - Adds --with-<name> and --with-<name>-libdir options to configure
# - Sanity checks directory names given to the above options (i.e.,
#   look for a token file in each, but only if the directory argument
#   is given)
# - Assumes that if --with-<name> is supplied and we can't build the
#   component, it's a fatal error.
# - Assumes that if --with-<name> is NOT supplied and we can't build
#   the component, it's NOT a fatal error.
# - Run OPAL_CHECK_PACKAGE (check for the specific presence of header
#   files and/or libraries) to determine if the package is available
# - Set and AC_SUBST <framework>_<component>_CPPFLAGS
# - Set and AC_SUBST <framework>_<component>_CFLAGS
# - Set and AC_SUBST <framework>_<component>_LDFLAGS
# - Set and AC_SUBST <framework>_<component>_LIBS
#
# Enjoy.
#
AC_DEFUN([OPAL_SETUP_COMPONENT_PACKAGE],[
    AC_ARG_WITH([$3],
        [AC_HELP_STRING([--with-$3(=DIR)],
                        [Build $3 support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([$3], [$with_$3], [$4])
    AC_ARG_WITH([$3-libdir],
        [AC_HELP_STRING([--with-$3-libdir=DIR],
                        [Search for the $3 libraries in DIR])])
    OPAL_CHECK_WITHDIR([$3-libdir], [$with_$3_libdir], [$5])

    AS_IF([test ! -z "$with_$3" && test "$with_$3" != "yes"],
          [$1_$2_dir="$with_$3"])
    AS_IF([test ! -z "$with_$3_libdir" && test "$with_$3_libdir" != "yes"],
          [$1_$2_libdir="$with_$3_libdir"])

    AS_IF([test "$with_$3" = "no"],
          [$1_$2_happy="no"],
          [$1_$2_happy="yes"])

    AS_IF([test "$$1_$2_happy" = "yes"],
          [OPAL_CHECK_PACKAGE([$1_$2],
                              [$6],
                              [$7],
                              [$8],
                              [$9],
                              [$$1_$2_dir],
                              [$$1_$2_libdir],
                              [$1_$2_happy="yes"],
                              [$1_$2_happy="no"])])

    AS_IF([test "$$1_$2_happy" = "yes"],
          [$10],
          [$11])

    # sanity check
    AS_IF([test "$$1_$2_happy" = "no"],
          [AS_IF([test "$with_$3" != "no" && test ! -z "$with_$3"],
                 [AC_MSG_WARN([$1:$2 requested but not found])
                  AC_MSG_ERROR([Cannot continue])])])

    AC_SUBST([$1_$2_CFLAGS])
    AC_SUBST([$1_$2_CPPFLAGS])
    AC_SUBST([$1_$2_LDFLAGS])
    AC_SUBST([$1_$2_LIBS])
])
