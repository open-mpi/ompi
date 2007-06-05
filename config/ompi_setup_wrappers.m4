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
dnl Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
dnl                         Use is subject to license terms.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


# OMPI_SETUP_WRAPPER_INIT()
# -------------------------
# Setup wrapper compiler configuration information - should be called
# before the bulk of the tests that can affect the wrapper compilers
#
# Note that we keep the user-specified flags seperately because we
# don't want them to go through OMPI_UNIQ because that has resulted in
# unexpected behavior for the user in the past.
AC_DEFUN([OMPI_SETUP_WRAPPER_INIT],[
    WRAPPER_EXTRA_CPPFLAGS=
    WRAPPER_EXTRA_CFLAGS=
    WRAPPER_EXTRA_CXXFLAGS=
    WRAPPER_EXTRA_FFLAGS=
    WRAPPER_EXTRA_FCFLAGS=
    WRAPPER_EXTRA_LDFLAGS=
    WRAPPER_EXTRA_LIBS=

    USER_WRAPPER_EXTRA_CFLAGS=
    USER_WRAPPER_EXTRA_CXXFLAGS=
    USER_WRAPPER_EXTRA_FFLAGS=
    USER_WRAPPER_EXTRA_FCFLAGS=
    USER_WRAPPER_EXTRA_LDFLAGS=
    USER_WRAPPER_EXTRA_LIBS=

    AC_ARG_WITH([wrapper-cflags], 
                [AC_HELP_STRING([--with-wrapper-cflags],
                                [Extra flags to add to CFLAGS when using mpicc])])
    if test "$with_wrapper_cflags" = "yes" -o "$with_wrapper_cflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cflags" ; then
        USER_WRAPPER_EXTRA_CFLAGS="$with_wrapper_cflags"
    fi

    AC_ARG_WITH([wrapper-cxxflags], 
                [AC_HELP_STRING([--with-wrapper-cxxflags],
                                [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
    if test "$with_wrapper_cxxflags" = "yes" -o "$with_wrapper_cxxflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-cxxflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_cxxflags" ; then
        USER_WRAPPER_EXTRA_CXXFLAGS="$with_wrapper_cxxflags"
    fi

    AC_ARG_WITH([wrapper-fflags], 
                [AC_HELP_STRING([--with-wrapper-fflags],
                                [Extra flags to add to FFLAGS when using mpif77])])
    if test "$with_wrapper_fflags" = "yes" -o "$with_wrapper_fflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-fflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_fflags" ; then
        USER_WRAPPER_EXTRA_FFLAGS="$with_wrapper_fflags"
    fi

    AC_ARG_WITH([wrapper-fcflags], 
                [AC_HELP_STRING([--with-wrapper-fcflags],
                                [Extra flags to add to FCFLAGS when using mpif90])])
    if test "$with_wrapper_fcflags" = "yes" -o "$with_wrapper_fcflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-fcflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_fcflags" ; then
        USER_WRAPPER_EXTRA_FCFLAGS="$with_wrapper_fcflags"
    fi

    AC_ARG_WITH([wrapper-ldflags], 
                [AC_HELP_STRING([--with-wrapper-ldflags],
                                [Extra flags to add to LDFLAGS when using wrapper compilers])])
    if test "$with_wrapper_ldflags" = "yes" -o "$with_wrapper_ldflags" = "no"; then
        AC_MSG_ERROR([--with-wrapper-ldflags must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_ldflags" ; then
        USER_WRAPPER_EXTRA_LDFLAGS="$with_wrapper_ldflags"
    fi

    AC_ARG_WITH([wrapper-libs], 
                [AC_HELP_STRING([--with-wrapper-libs],
                                [Extra flags to add to LIBS when using wrapper compilers])])
    if test "$with_wrapper_libs" = "yes" -o "$with_wrapper_libs" = "no"; then
        AC_MSG_ERROR([--with-wrapper-libs must have an argument.  Aborting])
    elif test ! -z "$with_wrapper_libs" ; then
        USER_WRAPPER_EXTRA_LIBS="$with_wrapper_libs"
    fi
])


AC_DEFUN([OMPI_SETUP_WRAPPER_FINAL],[
    OMPI_UNIQ([WRAPPER_EXTRA_CPPFLAGS])
    OMPI_UNIQ([WRAPPER_EXTRA_CFLAGS])
    OMPI_UNIQ([WRAPPER_EXTRA_CXXFLAGS])
    OMPI_UNIQ([WRAPPER_EXTRA_FFLAGS])
    OMPI_UNIQ([WRAPPER_EXTRA_FCFLAGS])
    OMPI_UNIQ([WRAPPER_EXTRA_LDFLAGS])

    #
    # OPAL
    #
    OMPI_UNIQ([opal_WRAPPER_EXTRA_LDFLAGS])
    OMPI_UNIQ([opal_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for OPAL CPPFLAGS])
    OPAL_WRAPPER_EXTRA_CPPFLAGS="$WRAPPER_EXTRA_CPPFLAGS $USER_WRAPPER_EXTRA_CPPFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CPPFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CPPFLAGS])

    AC_MSG_CHECKING([for OPAL CFLAGS])
    OPAL_WRAPPER_EXTRA_CFLAGS="$WRAPPER_EXTRA_CFLAGS $USER_WRAPPER_EXTRA_CFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CFLAGS])

    AC_MSG_CHECKING([for OPAL CXXFLAGS])
    OPAL_WRAPPER_EXTRA_CXXFLAGS="$WRAPPER_EXTRA_CXXFLAGS $USER_WRAPPER_EXTRA_CXXFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_CXXFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CXXFLAGS])

    AC_MSG_CHECKING([for OPAL LDFLAGS])
    OPAL_WRAPPER_EXTRA_LDFLAGS="$opal_WRAPPER_EXTRA_LDFLAGS $WRAPPER_EXTRA_LDFLAGS $USER_WRAPPER_EXTRA_LDFLAGS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_LDFLAGS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_LDFLAGS])

    AC_MSG_CHECKING([for OPAL LIBS])
    OPAL_WRAPPER_EXTRA_LIBS="$opal_WRAPPER_EXTRA_LIBS $WRAPPER_EXTRA_LIBS $USER_WRAPPER_EXTRA_LIBS"
    AC_SUBST([OPAL_WRAPPER_EXTRA_LIBS])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for OPAL extra include dirs])
    if test "$WANT_INSTALL_HEADERS" = "1" ; then
        OPAL_WRAPPER_EXTRA_INCLUDES="openmpi"
    else
        OPAL_WRAPPER_EXTRA_INCLUDES=
    fi
    AC_SUBST([OPAL_WRAPPER_EXTRA_INCLUDES])
    AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_INCLUDES])

    #
    # ORTE
    #
    OMPI_UNIQ([orte_WRAPPER_EXTRA_LDFLAGS])
    OMPI_UNIQ([orte_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for ORTE CPPFLAGS])
    ORTE_WRAPPER_EXTRA_CPPFLAGS="$OPAL_WRAPPER_EXTRA_CPPFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_CPPFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CPPFLAGS])

    AC_MSG_CHECKING([for ORTE CFLAGS])
    ORTE_WRAPPER_EXTRA_CFLAGS="$OPAL_WRAPPER_EXTRA_CFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_CFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CFLAGS])

    AC_MSG_CHECKING([for ORTE CXXFLAGS])
    ORTE_WRAPPER_EXTRA_CXXFLAGS="$OPAL_WRAPPER_EXTRA_CXXFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_CXXFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CXXFLAGS])

    AC_MSG_CHECKING([for ORTE LDFLAGS])
    ORTE_WRAPPER_EXTRA_LDFLAGS="$orte_WRAPPER_EXTRA_LDFLAGS $OPAL_WRAPPER_EXTRA_LDFLAGS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_LDFLAGS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_LDFLAGS])

    AC_MSG_CHECKING([for ORTE LIBS])
    ORTE_WRAPPER_EXTRA_LIBS="$orte_WRAPPER_EXTRA_LIBS $OPAL_WRAPPER_EXTRA_LIBS"
    AC_SUBST([ORTE_WRAPPER_EXTRA_LIBS])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for ORTE extra include dirs])
    ORTE_WRAPPER_EXTRA_INCLUDES="$OPAL_WRAPPER_EXTRA_INCLUDES"
    AC_SUBST([ORTE_WRAPPER_EXTRA_INCLUDES])
    AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_INCLUDES])

    #
    # OMPI
    #
    OMPI_UNIQ([ompi_WRAPPER_EXTRA_LDFLAGS])
    OMPI_UNIQ([ompi_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for OMPI CPPFLAGS])
    OMPI_WRAPPER_EXTRA_CPPFLAGS="$ORTE_WRAPPER_EXTRA_CPPFLAGS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_CPPFLAGS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CPPFLAGS])

    AC_MSG_CHECKING([for OMPI CFLAGS])
    OMPI_WRAPPER_EXTRA_CFLAGS="$ORTE_WRAPPER_EXTRA_CFLAGS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_CFLAGS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CFLAGS])

    AC_MSG_CHECKING([for OMPI CXXFLAGS])
    OMPI_WRAPPER_EXTRA_CXXFLAGS="$ORTE_WRAPPER_EXTRA_CXXFLAGS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_CXXFLAGS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CXXFLAGS])

    AC_MSG_CHECKING([for OMPI FFLAGS])
    OMPI_WRAPPER_EXTRA_FFLAGS="$WRAPPER_EXTRA_FFLAGS $USER_WRAPPER_EXTRA_FFLAGS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_FFLAGS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_FFLAGS])

    AC_MSG_CHECKING([for OMPI FCFLAGS])
    OMPI_WRAPPER_EXTRA_FCFLAGS="$WRAPPER_EXTRA_FCFLAGS $USER_WRAPPER_EXTRA_FCFLAGS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_FCFLAGS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_FCFLAGS])

    AC_MSG_CHECKING([for OMPI LDFLAGS])
    OMPI_WRAPPER_EXTRA_LDFLAGS="$ompi_WRAPPER_EXTRA_LDFLAGS $ORTE_WRAPPER_EXTRA_LDFLAGS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_LDFLAGS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_LDFLAGS])

    AC_MSG_CHECKING([for OMPI LIBS])
    OMPI_WRAPPER_EXTRA_LIBS="$ompi_WRAPPER_EXTRA_LIBS $ORTE_WRAPPER_EXTRA_LIBS"
    AC_SUBST([OMPI_WRAPPER_EXTRA_LIBS])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_LIBS])

    AC_MSG_CHECKING([for OMPI extra include dirs])
    OMPI_WRAPPER_EXTRA_INCLUDES="$ORTE_WRAPPER_EXTRA_INCLUDES"
    AC_SUBST([OMPI_WRAPPER_EXTRA_INCLUDES])
    AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_INCLUDES])

    # language binding support.  C++ is a bit different, as the
    # compiler should work even if there is no language support.
    if test "$WANT_MPI_CXX_SUPPORT" = "1" ; then
        OMPI_WRAPPER_CXX_LIB="-lmpi_cxx"
        OMPI_WRAPPER_CXX_REQUIRED_FILE=""
    else
        OMPI_WRAPPER_CXX_LIB=""
        OMPI_WRAPPER_CXX_REQUIRED_FILE=""
    fi
    AC_SUBST([OMPI_WRAPPER_CXX_LIB])
    AC_SUBST([OMPI_WRAPPER_CXX_REQUIRED_FILE])

    if test "$OMPI_WANT_F77_BINDINGS" = "1" ; then
        OMPI_WRAPPER_F77_REQUIRED_FILE=""
    else
        OMPI_WRAPPER_F77_REQUIRED_FILE="not supported"
    fi
    AC_SUBST([OMPI_WRAPPER_F77_REQUIRED_FILE])

    if test "$OMPI_WANT_F90_BINDINGS" = "1" ; then
        OMPI_WRAPPER_F90_REQUIRED_FILE=""
    else
        OMPI_WRAPPER_F90_REQUIRED_FILE="not supported"
    fi
    AC_SUBST([OMPI_WRAPPER_F90_REQUIRED_FILE])


    # compatibility defines that will eventually go away
    WRAPPER_EXTRA_CFLAGS="$OMPI_WRAPPER_EXTRA_CFLAGS"
    WRAPPER_EXTRA_CXXFLAGS="$OMPI_WRAPPER_EXTRA_CXXFLAGS"
    WRAPPER_EXTRA_FFLAGS="$OMPI_WRAPPER_EXTRA_FFLAGS"
    WRAPPER_EXTRA_FCFLAGS="$OMPI_WRAPPER_EXTRA_FCFLAGS"
    WRAPPER_EXTRA_LDFLAGS="$OMPI_WRAPPER_EXTRA_LDFLAGS"
    WRAPPER_EXTRA_LIBS="$OMPI_WRAPPER_EXTRA_LIBS"

    AC_SUBST(WRAPPER_EXTRA_CFLAGS)
    AC_SUBST(WRAPPER_EXTRA_CXXFLAGS)
    AC_SUBST(WRAPPER_EXTRA_FFLAGS)
    AC_SUBST(WRAPPER_EXTRA_FCFLAGS)
    AC_SUBST(WRAPPER_EXTRA_LDFLAGS)
    AC_SUBST(WRAPPER_EXTRA_LIBS)

    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$WRAPPER_EXTRA_CFLAGS",
        [Additional CFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS, "$WRAPPER_EXTRA_CXXFLAGS",
        [Additional CXXFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FFLAGS, "$WRAPPER_EXTRA_FFLAGS",
        [Additional FFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FCFLAGS, "$WRAPPER_EXTRA_FCFLAGS",
        [Additional FCFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LDFLAGS, "$WRAPPER_EXTRA_LDFLAGS",
        [Additional LDFLAGS to pass through the wrapper compilers])
    AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LIBS, "$WRAPPER_EXTRA_LIBS",
        [Additional LIBS to pass through the wrapper compilers])
])
