dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2006 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2012 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2007-2012 Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_JAVA_BINDINGS_BANNER],[
    opal_show_subtitle "Java MPI bindings"
])

# OMPI_SETUP_JAVA_BINDINGS()
# ----------------
# Do everything required to setup the Java MPI bindings.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OMPI_SETUP_JAVA_BINDINGS],[
    # must have Java setup
    AC_REQUIRE([OPAL_SETUP_JAVA])

    AC_REQUIRE([OMPI_SETUP_JAVA_BINDINGS_BANNER])

    AC_MSG_CHECKING([if want Java bindings])
    AC_ARG_ENABLE(mpi-java,
        AC_HELP_STRING([--enable-mpi-java],
                       [enable Java MPI bindings (default: disabled)]))

    # check for required support
    if test "$opal_java_happy" = "no" && test "$enable_mpi_java" = "yes"; then
        AC_MSG_RESULT([yes])
        AC_MSG_WARN([Java bindings requested but no Java support found])
        AC_MSG_ERROR([cannot continue])
    fi

    # Only build the Java bindings if requested
    if test "$opal_java_happy" = "yes" && test "$enable_mpi_java" = "yes"; then
        AC_MSG_RESULT([yes])
        WANT_MPI_JAVA_SUPPORT=1
        AC_MSG_CHECKING([if shared libraries are enabled])
        AS_IF([test "$enable_shared" != "yes"],
              [AC_MSG_RESULT([no])
               AC_MSG_WARN([Java bindings cannot be built without shared libraries])
               AC_MSG_WARN([Please reconfigure with --enable-shared])
               AC_MSG_ERROR([Cannot continue])],
              [AC_MSG_RESULT([yes])])
        # must have Java support
        AC_MSG_CHECKING([if Java support was found])
        AS_IF([test "$opal_java_happy" = "yes"],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_WARN([Java MPI bindings requested, but Java support was not found])
               AC_MSG_WARN([Please reconfigure the --with-jdk options to where Java])
               AC_MSG_WARN([support can be found])
               AC_MSG_ERROR([Cannot continue])])

        # Mac Java requires this file (i.e., some other Java-related
        # header file needs this file, so we need to check for
        # it/include it in our sources when compiling on Mac).
        AC_CHECK_HEADERS([TargetConditionals.h])

        # dladdr and Dl_info are required to build the full path to libmpi on OS X 10.11 aka El Capitan
        AC_CHECK_TYPES([Dl_info], [], [], [[#include <dlfcn.h>]])
    else
        AC_MSG_RESULT([no])
        WANT_MPI_JAVA_SUPPORT=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_WANT_JAVA_BINDINGS], [$WANT_MPI_JAVA_SUPPORT],
                       [do we want java mpi bindings])
    AM_CONDITIONAL(OMPI_WANT_JAVA_BINDINGS, test "$WANT_MPI_JAVA_SUPPORT" = "1")

   # Are we happy?
    AS_IF([test "$WANT_MPI_JAVA_SUPPORT" = "1"],
          [AC_MSG_WARN([******************************************************])
           AC_MSG_WARN([*** Java MPI bindings are provided on a provisional])
           AC_MSG_WARN([*** basis.  They are NOT part of the current or])
           AC_MSG_WARN([*** proposed MPI standard.  Continued inclusion of])
           AC_MSG_WARN([*** the Java MPI bindings in Open MPI is contingent])
           AC_MSG_WARN([*** upon user interest and developer support.])
           AC_MSG_WARN([******************************************************])
          ])

    AC_CONFIG_FILES([
        ompi/mpi/java/Makefile
        ompi/mpi/java/java/Makefile
        ompi/mpi/java/c/Makefile
    ])
])
