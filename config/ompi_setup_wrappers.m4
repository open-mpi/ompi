dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


AC_DEFUN([OMPI_SETUP_WRAPPER_INIT],[
    WRAPPER_EXTRA_CFLAGS=
    WRAPPER_EXTRA_CXXFLAGS=
    WRAPPER_EXTRA_FFLAGS=
    WRAPPER_EXTRA_FCFLAGS=
    WRAPPER_EXTRA_LDFLAGS=
    WRAPPER_EXTRA_LIBS=

    AC_ARG_WITH([wrapper-cflags], 
                [AC_HELP_STRING([--with-wrapper-cflags],
                                [Extra flags to add to CFLAGS when using mpicc])])
    if test "$with_wrapper_cflags" = "yes" -o "$with_wrapper_cflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cflags" ; then
        WRAPPER_EXTRA_CFLAGS="$with_wrapper_cflags"
    fi

    AC_ARG_WITH([wrapper-cxxflags], 
                [AC_HELP_STRING([--with-wrapper-cxxflags],
                                [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
    if test "$with_wrapper_cxxflags" = "yes" -o "$with_wrapper_cxxflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cxxflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cxxflags" ; then
        WRAPPER_EXTRA_CXXFLAGS="$with_wrapper_cxxflags"
    fi

    AC_ARG_WITH([wrapper-fflags], 
                [AC_HELP_STRING([--with-wrapper-fflags],
                                [Extra flags to add to FFLAGS when using mpif77])])
    if test "$with_wrapper_fflags" = "yes" -o "$with_wrapper_fflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-fflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_fflags" ; then
        WRAPPER_EXTRA_FFLAGS="$with_wrapper_fflags"
    fi

    AC_ARG_WITH([wrapper-fcflags], 
                [AC_HELP_STRING([--with-wrapper-fcflags],
                                [Extra flags to add to FCFLAGS when using mpif90])])
    if test "$with_wrapper_fcflags" = "yes" -o "$with_wrapper_fcflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-fcflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_fcflags" ; then
        WRAPPER_EXTRA_FCFLAGS="$with_wrapper_fcflags"
    fi

    AC_ARG_WITH([wrapper-ldflags], 
                [AC_HELP_STRING([--with-wrapper-ldflags],
                                [Extra flags to add to LDFLAGS when using wrapper compilers])])
    if test "$with_wrapper_ldflags" = "yes" -o "$with_wrapper_ldflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-ldflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_ldflags" ; then
        WRAPPER_EXTRA_LDFLAGS="$with_wrapper_ldflags"
    fi

    AC_ARG_WITH([wrapper-libs], 
                [AC_HELP_STRING([--with-wrapper-libs],
                                [Extra flags to add to LIBS when using wrapper compilers])])
    if test "$with_wrapper_libs" = "yes" -o "$with_wrapper_libs" = "no"; then
        AC_MSG_ERROR([--with-wrapper-libs must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_libs" ; then
        WRAPPER_EXTRA_LIBS="$with_wrapper_libs"
    fi
])


AC_DEFUN([OMPI_SETUP_WRAPPER_FINAL],[
    #
    # Adding WRAPPER_* flags so that extra flags needed for wrappper compilers
    #
    # WRAPPER_EXTRA_CFLAGS
    #
    AC_MSG_CHECKING([for mpicc CFLAGS])
    OMPI_UNIQ(WRAPPER_EXTRA_CFLAGS)
    AC_SUBST(WRAPPER_EXTRA_CFLAGS)
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$WRAPPER_EXTRA_CFLAGS",
        [Additional CFLAGS to pass through the wrapper compilers])
    AC_MSG_RESULT([$WRAPPER_EXTRA_CFLAGS])

    #
    # WRAPPER_EXTRA_CXXFLAGS
    #
    AC_MSG_CHECKING([for mpiCC CXXFLAGS])
    OMPI_UNIQ(WRAPPER_EXTRA_CXXFLAGS)
    AC_SUBST(WRAPPER_EXTRA_CXXFLAGS)
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS, "$WRAPPER_EXTRA_CXXFLAGS",
        [Additional CXXFLAGS to pass through the wrapper compilers])
    AC_MSG_RESULT([$WRAPPER_EXTRA_CXXFLAGS])

    #
    # WRAPPER_EXTRA_FFLAGS
    #
    AC_MSG_CHECKING([for mpif77/mpif90 FFLAGS])
    OMPI_UNIQ(WRAPPER_EXTRA_FFLAGS)
    AC_SUBST(WRAPPER_EXTRA_FFLAGS)
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FFLAGS, "$WRAPPER_EXTRA_FFLAGS",
        [Additional FFLAGS to pass through the wrapper compilers])
    AC_MSG_RESULT([$WRAPPER_EXTRA_FFLAGS])

    #
    # WRAPPER_EXTRA_FCFLAGS
    #
    AC_MSG_CHECKING([for mpif77/mpif90 FCFLAGS])
    OMPI_UNIQ(WRAPPER_EXTRA_FCFLAGS)
    AC_SUBST(WRAPPER_EXTRA_FCFLAGS)
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FCFLAGS, "$WRAPPER_EXTRA_FCFLAGS",
        [Additional FCFLAGS to pass through the wrapper compilers])
    AC_MSG_RESULT([$WRAPPER_EXTRA_FCFLAGS])

    #
    # WRAPPER_EXTRA_LDFLAGS
    #
    AC_MSG_CHECKING([for wrapper compiler LDFLAGS])
    OMPI_UNIQ(WRAPPER_EXTRA_LDFLAGS)
    AC_SUBST(WRAPPER_EXTRA_LDFLAGS)
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LDFLAGS, "$WRAPPER_EXTRA_LDFLAGS",
        [Additional LDFLAGS to pass through the wrapper compilers])
    AC_MSG_RESULT([$WRAPPER_EXTRA_LDFLAGS])

    #
    # WRAPPER_EXTRA_LIBS
    #
    AC_MSG_CHECKING([for wrapper compiler LIBS])
    AC_SUBST(WRAPPER_EXTRA_LIBS)
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LIBS, "$WRAPPER_EXTRA_LIBS",
        [Additional LIBS to pass through the wrapper compilers])
    AC_MSG_RESULT([$WRAPPER_EXTRA_LIBS])
])
