dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2018      DataDirect Networks. All rights reserved.
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

    check_ime_CPPFLAGS=
    check_ime_LDFLAGS=
    check_ime_LIBS=

    check_ime_configuration="none"
    ompi_check_ime_happy="yes"


    # Get some configuration information
    AC_ARG_WITH([ime],
        [AC_HELP_STRING([--with-ime(=DIR)],
             [Build IME support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([ime], [$with_ime], [include/ime_native.h])

    AS_IF([test "$with_ime" = "no"],
        [ompi_check_ime_happy="no"],
        [AS_IF([test -z "$with_ime"],
                [ompi_check_ime_dir="/usr/local"],
                [ompi_check_ime_dir=$with_ime])

            if test -e "$ompi_check_ime_dir/lib64" ; then
                ompi_check_ime_libdir="$ompi_check_ime_dir/lib64"
            else
                ompi_check_ime_libdir="$ompi_check_ime_dir/lib"
            fi

            # Add correct -I and -L flags
            OPAL_CHECK_PACKAGE([$1], [ime_native.h], [im_client], [ime_client_native2_init], [],
                [$ompi_check_ime_dir], [$ompi_check_ime_libdir], 
                [ompi_check_ime_happy="yes"],
                [OPAL_CHECK_PACKAGE([$1], [ime_native.h], [im_client], [ime_native_init], [],
                    [$ompi_check_ime_dir], [$ompi_check_ime_libdir], 
                    [ompi_check_ime_happy="yes"],
                    [ompi_check_ime_happy="no"])
                ])
        ])

    AS_IF([test "$ompi_check_ime_happy" = "yes"],
        [$2],
        [AS_IF([test ! -z "$with_ime" && test "$with_ime" != "no"],
                [echo IME support not found])
            $3])
    
    ])

