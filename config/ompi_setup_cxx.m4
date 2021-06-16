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
dnl Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015-2016 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2021      Nanook Consulting.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl This macro is necessary to get the title to be displayed first.  :-)
AC_DEFUN([OMPI_SETUP_CXX_BANNER],[
    opal_show_subtitle "C++ compiler and preprocessor"
])

AC_DEFUN([OMPI_PROG_CXX],[
    OPAL_VAR_SCOPE_PUSH([ompi_cxxflags_save ompi_cxx_argv0])

    ompi_cxxflags_save="$CXXFLAGS"
    AC_PROG_CXX
    CXXFLAGS="$ompi_cxxflags_save"

    # Note: according to the Autoconf docs, if no C++ compiler is
    # found, $CXX is still set to "g++" (!!).  So make sure that we
    # actually found a C++ compiler; if not, set CXX to "no", per
    # thread at
    # https://www.open-mpi.org/community/lists/users/2013/02/21356.php,
    # which advises us to set Libtool precious variables to "no" if we
    # don't want Libtool to setup that language at all.
    set dummy $CXX
    ompi_cxx_argv0=[$]2
    OPAL_WHICH([$ompi_cxx_argv0], [OMPI_CXX_ABSOLUTE])
    AS_IF([test "x$OMPI_CXX_ABSOLUTE" = "x"],
          [CXX=no
           OMPI_CXX_ABSOLUTE=no],
          [ # If we did actually find a C++ compiler, find the C++ CPP
           AC_PROG_CXXCPP])

    AC_DEFINE_UNQUOTED(OMPI_CXX, "$CXX", [OMPI underlying C++ compiler])
    AC_SUBST(OMPI_CXX_ABSOLUTE)

    AM_CONDITIONAL([OMPI_HAVE_CXX_COMPILER], [test "$CXX" != "no"])

    OPAL_VAR_SCOPE_POP
])

dnl OMPI_SETUP_CXX()
dnl ----------------
dnl Do everything required to setup the C++ compiler for the mpic++
dnl wrapper compiler (there is no C++ code in Open MPI, so we do not
dnl need to setup for internal C++ compilations).  Safe to AC_REQUIRE
dnl this macro.
AC_DEFUN([OMPI_SETUP_CXX],[
    # Do a little tomfoolery to get the subsection title printed first
    AC_REQUIRE([OMPI_SETUP_CXX_BANNER])

    # Must REQUIRE the PROG_CXX macro and not call it directly here
    # for reasons well-described in the AC2.64 (and beyond) docs --
    # see the docs for AC PROG_CC for details.
    AC_REQUIRE([OMPI_PROG_CXX])

    # If we have a C++ compiler, do some additional tests
    AS_IF([test "$CXX" != "no"],
        [ # Make sure we can link with the C compiler
         OPAL_LANG_LINK_WITH_C([C++], [],
            [cat <<EOF >&2
**********************************************************************
* It appears that your C++ compiler is unable to link against object
* files created by your C compiler.  This generally indicates either
* a conflict between the options specified in CFLAGS and CXXFLAGS
* or a problem with the local compiler installation.  More
* information (including exactly what command was given to the
* compilers and what error resulted when the commands were executed) is
* available in the config.log file in this directory.
**********************************************************************
EOF
             AC_MSG_ERROR([C and C++ compilers are not link compatible.  Can not continue.])])
        ])
])
