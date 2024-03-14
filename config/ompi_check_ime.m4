dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2018      DataDirect Networks. All rights reserved.
dnl Copyright (c) 2021      Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_IME(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if IME support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_IME],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_ime_happy])

    # Get some configuration information
    AC_ARG_WITH([ime],
        [AS_HELP_STRING([--with-ime(=DIR)],
             [Build IME support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])

    OAC_CHECK_PACKAGE([ime],
                      [$1],
                      [ime_native.h],
                      [im_client],
                      [ime_client_native2_init],
                      [ompi_check_ime_happy="yes"],
                      [ompi_check_ime_happy="no"])

    AS_IF([test "$ompi_check_ime_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ime" && test "$with_ime" != "no"],
                 [AC_MSG_ERROR([IME support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])

