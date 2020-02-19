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
    OPAL_VAR_SCOPE_PUSH([ompi_cxxflags_save])
    ompi_cxxflags_save="$CXXFLAGS"
    AC_PROG_CXX
    AC_PROG_CXXCPP
    CXXFLAGS="$ompi_cxxflags_save"
    OPAL_VAR_SCOPE_POP
])

dnl OMPI_SETUP_CXX()
dnl ----------------
dnl Do everything required to setup the C++ compiler for the mpic++
dnl wrapper compiler (there is no C++ code in Open MPI, so we do not
dnl need to setup for internal C++ compilations).  Safe to AC_REQUIRE
dnl this macro.
AC_DEFUN([OMPI_SETUP_CXX],[
    OPAL_VAR_SCOPE_PUSH([ompi_cxx_argv0])

    # Do a little tomfoolery to get the subsection title printed first
    AC_REQUIRE([OMPI_SETUP_CXX_BANNER])

    # Must REQUIRE the PROG_CXX macro and not call it directly here
    # for reasons well-described in the AC2.64 (and beyond) docs --
    # see the docs for AC PROG_CC for details.
    AC_REQUIRE([OMPI_PROG_CXX])

    BASECXX="`basename $CXX`"

    AS_IF([test "x$CXX" = "x"], [CXX=none])
    set dummy $CXX
    ompi_cxx_argv0=[$]2
    OPAL_WHICH([$ompi_cxx_argv0], [OMPI_CXX_ABSOLUTE])
    AS_IF([test "x$OMPI_CXX_ABSOLUTE" = "x"], [OMPI_CXX_ABSOLUTE=none])

    AC_DEFINE_UNQUOTED(OMPI_CXX, "$CXX", [OMPI underlying C++ compiler])
    AC_SUBST(OMPI_CXX_ABSOLUTE)

    # Make sure we can link with the C compiler
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

    # bool type size and alignment
    AC_LANG_PUSH(C++)
    AC_CHECK_SIZEOF(bool)
    OPAL_C_GET_ALIGNMENT(bool, OPAL_ALIGNMENT_CXX_BOOL)
    AC_LANG_POP(C++)

    OPAL_VAR_SCOPE_POP
])
