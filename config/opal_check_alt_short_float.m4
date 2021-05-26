dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2018-2021 FUJITSU LIMITED.  All rights reserved.
dnl Copyright (c) 2020 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl Check whether the user wants to use an alternate type of C 'short float'.

dnl OPAL_CHECK_ALT_SHORT_FLOAT
dnl ------------------------------------------------------------
AC_DEFUN([OPAL_CHECK_ALT_SHORT_FLOAT], [
    AC_CHECK_TYPES(_Float16)
    AC_MSG_CHECKING([if want alternate C type of short float])
    AC_ARG_ENABLE([alt-short-float],
        [AS_HELP_STRING([--enable-alt-short-float=TYPE],
                       [Use an alternate C type TYPE of 'short float' if 'short float' is not available on the C compiler. 'short float' is a new C type proposed for the next C language standard in ISO/IEC JTC 1/SC 22 WG 14 (C WG). (default: "_Float16" if available, disabled otherwise)])])
    if test "$enable_alt_short_float" = "yes"; then
        AC_MSG_ERROR([--enable-alt-short-float must have an argument.])
    elif test "$enable_alt_short_float" = "no"; then
        :
    elif test "$enable_alt_short_float" != ""; then
        opal_short_float_type="$enable_alt_short_float"
        opal_short_float_complex_type="$enable_alt_short_float [[2]]"
    elif test "$ac_cv_type_short_float" = "yes" && \
         test "$ac_cv_type_short_float__Complex" = "yes"; then
        opal_short_float_type="short float"
        opal_short_float_complex_type="short float _Complex"
    elif test "$ac_cv_type__Float16" = "yes"; then
        opal_short_float_type="_Float16"
        opal_short_float_complex_type="_Float16 [[2]]"
    fi
    if test "$opal_short_float_type" != ""; then
        AC_MSG_RESULT([yes ($opal_short_float_type)])
        AC_CHECK_TYPES($opal_short_float_type, [opal_alt_short_float_exists=1], [opal_alt_short_float_exists=0])

        # Even if the alternate short float type exists, make sure
        # that the compiler can actually compile/link when
        # mathematical operations are performed on variables of that
        # type.  Case in point: clang 6.0.x and 7.0.x need an
        # additional CLI flag added (--rtlib=compiler-rt) to enable
        # software emulation of _Float16.  Open MPI will *not*
        # automagically add that flag -- we'll just emit a warning and
        # point the user to a README where more information is
        # available.
        AS_IF([test $opal_alt_short_float_exists -eq 1],
              [AC_MSG_CHECKING([if compiler supports arithmetic operations on $opal_short_float_type])
               AC_LINK_IFELSE([AC_LANG_PROGRAM([], [[
static $opal_short_float_type a = 2.5, b = 3.8;
a += b;]])],
                                 [AC_MSG_RESULT([yes])
                                  opal_enable_short_float=1],
                                 [AC_MSG_RESULT([no])
                                  AS_IF([test `basename $CC` = "clang"],
                                        [AC_MSG_WARN([if you are using the Clang 6.0.x or 7.0.x compilers and want])
                                         AC_MSG_WARN([to enable software emulation of half-precision floating point])
                                         AC_MSG_WARN([in conjunction with the "shortfloat" Open MPI extension,])
                                         AC_MSG_WARN([see the ompi/mpiext/shortfloat/README.txt file for details.])
                                         ])
                                  opal_enable_short_float=0])
               ])

        if test "$opal_enable_short_float" = 1; then
            AC_DEFINE_UNQUOTED(opal_short_float_t, [[$opal_short_float_type]],
                               [User-selected alternate C type of short float])
            AC_DEFINE_UNQUOTED(opal_short_float_complex_t, [[$opal_short_float_complex_type]],
                               [User-selected alternate C type of short float _Complex])
            AC_CHECK_TYPES(opal_short_float_t)
            AC_CHECK_TYPES(opal_short_float_complex_t)
            AC_CHECK_SIZEOF(opal_short_float_t)
            AC_CHECK_SIZEOF(opal_short_float_complex_t)
            OPAL_C_GET_ALIGNMENT(opal_short_float_t, OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T)

            # Some versions of GCC (around 9.1.0?) emit a warning for _Float16
            # when compiling with -pedantic. Using __extension__ can suppress
            # the warning. The warning can be detected by -Werror in configure.
            # See https://github.com/open-mpi/ompi/issues/8840
            AC_MSG_CHECKING([if $opal_short_float_type needs __extension__ keyword])
            opal_alt_short_float_needs_extension=0
            OPAL_VAR_SCOPE_PUSH([CFLAGS_save])
            CFLAGS_save=$CFLAGS
            CFLAGS="-Werror $CFLAGS"
            AC_COMPILE_IFELSE([AC_LANG_SOURCE([$opal_short_float_type a;])],
                              [AC_MSG_RESULT([no])],
                              [AC_COMPILE_IFELSE([AC_LANG_SOURCE([__extension__ $opal_short_float_type a;])],
                                                 [opal_alt_short_float_needs_extension=1
                                                  AC_MSG_RESULT([yes])],
                                                 [AC_MSG_RESULT([no])])])
            CFLAGS=$CFLAGS_save
            OPAL_VAR_SCOPE_POP
            AC_DEFINE_UNQUOTED(OPAL_SHORT_FLOAT_TYPE, [[$opal_short_float_type]],
                               [User-selected alternate C type of short float (used to redefine opal_short_float_t in opal_bottom.h)])
            AC_DEFINE_UNQUOTED(OPAL_SHORT_FLOAT_NEEDS_EXTENSION,
                               [$opal_alt_short_float_needs_extension],
                               [Whether $opal_short_float_type needs __extension__ keyword])
        elif test "$enable_alt_short_float" != ""; then
            AC_MSG_ERROR([Alternate C type of short float $opal_short_float_type requested but not available.  Aborting])
        fi
    else
        AC_MSG_RESULT([no])
    fi
])
