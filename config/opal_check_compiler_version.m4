dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2021 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OPAL_CHECK_COMPILER_VERSION_ID()
# ----------------------------------------------------
# Try to figure out the compiler's name and version to detect cases,
# where users compile Open MPI with one version and compile the application
# with a different compiler.
#
AC_DEFUN([OPAL_CHECK_COMPILER_VERSION_ID],
[
    OPAL_CHECK_COMPILER(FAMILYID)
    OPAL_CHECK_COMPILER(VERSION)
])dnl


AC_DEFUN([OPAL_CHECK_COMPILER], [
    OAC_ASSERT_LITERAL([$1], [1])dnl
    lower=m4_tolower([$1])
    AC_CACHE_CHECK([for compiler $lower], [opal_cv_compiler_$1],
    [
            CPPFLAGS_orig=$CPPFLAGS
            CPPFLAGS="-I${OPAL_TOP_SRCDIR}/opal/include $CPPFLAGS"
            AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <stdlib.h>
#include "opal/opal_portable_platform.h"
]],[[
    FILE * f;
    f=fopen("conftestval", "w");
    if (!f) exit(1);
    fprintf (f, "%d", PLATFORM_COMPILER_$1);
            ]])], [
                opal_cv_compiler_$1=`cat conftestval`
            ], [
                opal_cv_compiler_$1=0
            ], [
                opal_cv_compiler_$1=0
            ])
            CPPFLAGS=$CPPFLAGS_orig
    ])
    AC_DEFINE_UNQUOTED([OPAL_BUILD_PLATFORM_COMPILER_$1], [$opal_cv_compiler_$1],
                       [The compiler $lower which OMPI was built with])
])dnl
