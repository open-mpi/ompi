dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

m4_define(MCA_memcpy_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_memcpy_CONFIG],[
        memcpy_base_include=

        # first, compile all the components
        MCA_CONFIGURE_FRAMEWORK($1, $2)

        # someone should have set this...
        if test "$memcpy_base_include" = "" ; then
            memcpy_base_include="base/memcpy_base_default.h"
        fi

        AC_CONFIG_LINKS([opal/mca/memcpy/base/base_impl.h:opal/mca/memcpy/$memcpy_base_include])
])
