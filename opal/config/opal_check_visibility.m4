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
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OPAL_CHECK_VISIBILITY
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_VISIBILITY],[
    AC_REQUIRE([AC_PROG_GREP])

    # Check if the compiler has support for visibility, like some versions of gcc, icc Sun Studio cc.
    AC_ARG_ENABLE(visibility, 
        AC_HELP_STRING([--enable-visibility],
            [enable visibility feature of certain compilers/linkers (default: enabled)]))
    if test "$enable_visibility" = "no"; then
        AC_MSG_CHECKING([whether to enable symbol visibility])
        AC_MSG_RESULT([no (disabled)]) 
        have_visibility=0
    else
        CFLAGS_orig="$CFLAGS"
        add=

        # check using gcc -fvisibility=hidden flag
        CFLAGS="$CFLAGS_orig -fvisibility=hidden"
        AC_CACHE_CHECK([if $CC supports -fvisibility],
            [opal_cv_cc_fvisibility],
            [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
                    __attribute__((visibility("default"))) int foo;
                    ]],[[int i;]])],
                    [opal_cv_cc_fvisibility=yes],
                    [if test -s conftest.err ; then
                        $GREP -iq "visibility" conftest.err
                        if test "$?" = "0" ; then
                            opal_cv_cc_fvisibility=no
                        else
                            opal_cv_cc_fvisibility=yes
                        fi
                     else
                        opal_cv_cc_fvisibility=yes
                     fi],
                    [opal_cv_cc_fvisibility=no])
                ])
        if test "$ompi_c_vendor" = "sun" ; then
            # Check using Sun Studio -xldscope=hidden flag
            CFLAGS="$CFLAGS_orig -xldscope=hidden"
            AC_CACHE_CHECK([if $CC supports -xldscope],
                [opal_cv_cc_xldscope],
                [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
                        __attribute__((visibility("default"))) int foo;
                        ]],[[int i;]])],
                        [opal_cv_cc_fvisibilty=yes],
                        [if test -s conftest.err ; then
                            $GREP -iq "visibility" conftest.err
                            if test "$?" = "0" ; then
                                opal_cv_cc_xldscope="no"
                            else
                                opal_cv_cc_xldscope="yes"
                            fi
                         else
                            opal_cv_cc_xldscope="yes"
                         fi],
                        [opal_cv_cc_xldscope="no"])
                    ])
        fi 
        if test "$opal_cv_cc_fvisibility" = "yes" ; then
            add=" -fvisibility=hidden"
            have_visibility=1
            AC_MSG_CHECKING([whether to enable symbol visibility])
            AC_MSG_RESULT([yes (via$add)]) 
            AC_MSG_WARN([$add has been added to CFLAGS])
        elif test "$opal_cv_cc_xldscope" = "yes" ; then
            add=" -xldscope=hidden"
            have_visibility=1
            AC_MSG_CHECKING([whether to enable symbol visibility])
            AC_MSG_RESULT([yes (via$add)]) 
            AC_MSG_WARN([$add has been added to CFLAGS])
        elif test "$enable_visibility" = "yes"; then
            AC_MSG_ERROR([Symbol visibility support requested but compiler does not seem to support it.  Aborting])
        else 
            AC_MSG_CHECKING([whether enable symbol visibility])
            AC_MSG_RESULT([no (unsupported)]) 
            have_visibility=0
        fi
        CFLAGS="$CFLAGS_orig$add"
        OPAL_VISIBILITY_CFLAGS="$add"
        unset add 
    fi

    AC_DEFINE_UNQUOTED([OPAL_C_HAVE_VISIBILITY], [$have_visibility],
            [Whether C compiler supports -fvisibility])
])
