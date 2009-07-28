# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_VISIBILITY
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_VISIBILITY],[
    AC_REQUIRE([AC_PROG_GREP])

    # Check if the compiler has support for visibility, like some versions of gcc, icc.
    AC_ARG_ENABLE(visibility, 
        AC_HELP_STRING([--enable-visibility],
            [enable visibility feature of certain compilers/linkers (default: enabled)]))
    if test "$enable_visibility" = "no"; then
        AC_MSG_CHECKING([enable symbol visibility])
        AC_MSG_RESULT([no]) 
        have_visibility=0
    else
        CFLAGS_orig="$CFLAGS"
        CFLAGS="$CFLAGS_orig -fvisibility=hidden"
        add=
        AC_CACHE_CHECK([if $CC supports -fvisibility],
            [ompi_cv_cc_fvisibility],
            [AC_TRY_LINK([
                    #include <stdio.h>
                    __attribute__((visibility("default"))) int foo;
                    void bar(void) { fprintf(stderr, "bar\n"); };
                    ],[],
                    [if test -s conftest.err ; then
                        $GREP -iq "visibility" conftest.err
                        if test "$?" = "0" ; then
                            ompi_cv_cc_fvisibility="no"
                        else
                            ompi_cv_cc_fvisibility="yes"
                        fi
                     else
                        ompi_cv_cc_fvisibility="yes"
                     fi],
                    [ompi_cv_cc_fvisibility="no"])
                ])

        if test "$ompi_cv_cc_fvisibility" = "yes" ; then
            add=" -fvisibility=hidden"
            have_visibility=1
            AC_MSG_CHECKING([enable symbol visibility])
            AC_MSG_RESULT([yes]) 
            AC_MSG_WARN([$add has been added to CFLAGS])
        elif test "$enable_visibility" = "yes"; then
            AC_MSG_ERROR([Symbol visibility support requested but compiler does not seem to support it.  Aborting])
        else 
            AC_MSG_CHECKING([enable symbol visibility])
            AC_MSG_RESULT([no]) 
            have_visibility=0
        fi
        CFLAGS="$CFLAGS_orig$add"
        OMPI_VISIBILITY_CFLAGS="$add"
        unset add 
    fi
    AC_DEFINE_UNQUOTED([OMPI_C_HAVE_VISIBILITY], [$have_visibility],
            [Whether C compiler supports -fvisibility])

])
