dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2021 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# PMIX_CHECK_COMPILER_VERSION_ID()
# ----------------------------------------------------
# Try to figure out the compiler's name and version to detect cases,
# where users compile Open MPI with one version and compile the application
# with a different compiler.
#
AC_DEFUN([PMIX_CHECK_COMPILER_VERSION_ID],
[
    PMIX_CHECK_COMPILER(FAMILYID)
    PMIX_CHECK_COMPILER(VERSION)
])dnl


AC_DEFUN([PMIX_CHECK_COMPILER], [
    AS_LITERAL_IF([$1], [],
                  [m4_fatal([PMIX_CHECK_COMPILER argument must be a literal])])
    lower=m4_tolower([$1])
    AC_CACHE_CHECK([for compiler $lower], [pmix_cv_compiler_$1],
    [
            CPPFLAGS_orig=$CPPFLAGS
            CPPFLAGS="-I${PMIX_TOP_SRCDIR}/pmix/include $CPPFLAGS"
            AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <stdlib.h>
#include "src/include/pmix_portable_platform.h"
]],[[
    FILE * f;
    f=fopen("conftestval", "w");
    if (!f) exit(1);
    fprintf (f, "%d", PLATFORM_COMPILER_$1);
            ]])], [
                pmix_cv_compiler_$1=`cat conftestval`
            ], [
                pmix_cv_compiler_$1=0
            ], [
                pmix_cv_compiler_$1=0
            ])
            CPPFLAGS=$CPPFLAGS_orig
    ])
    AC_DEFINE_UNQUOTED([PMIX_BUILD_PLATFORM_COMPILER_$1], [$pmix_cv_compiler_$1],
                       [The compiler $lower which OMPI was built with])
])dnl
