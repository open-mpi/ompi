dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
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
    OPAL_CHECK_COMPILER_STRINGIFY(FAMILYNAME)
    OPAL_CHECK_COMPILER(VERSION)
    OPAL_CHECK_COMPILER_STRING(VERSION_STR)
])dnl


AC_DEFUN([OPAL_CHECK_COMPILER], [
    lower=m4_tolower($1)
    AC_CACHE_CHECK([for compiler $lower], opal_cv_compiler_[$1],
    [
            CPPFLAGS_orig=$CPPFLAGS
            CPPFLAGS="-I${top_ompi_srcdir}/opal/include/opal $CPPFLAGS"
            AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include "opal_portable_platform.h"

int main (int argc, char * argv[])
{
    FILE * f;
    f=fopen("conftestval", "w");
    if (!f) exit(1);
    fprintf (f, "%d", PLATFORM_COMPILER_$1);
    return 0;
}
            ], [
                eval opal_cv_compiler_$1=`cat conftestval`;
            ], [
                eval opal_cv_compiler_$1=0
            ], [
                eval opal_cv_compiler_$1=0
            ])
            CPPFLAGS=$CPPFLAGS_orig
    ])
    AC_DEFINE_UNQUOTED([OPAL_BUILD_PLATFORM_COMPILER_$1], $opal_cv_compiler_[$1],
                       [The compiler $lower which OMPI was built with])
])dnl

AC_DEFUN([OPAL_CHECK_COMPILER_STRING], [
    lower=m4_tolower($1)
    AC_CACHE_CHECK([for compiler $lower], opal_cv_compiler_[$1],
    [
            CPPFLAGS_orig=$CPPFLAGS
            CPPFLAGS="-I${top_ompi_srcdir}/opal/include/opal $CPPFLAGS"
            AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include "opal_portable_platform.h"

int main (int argc, char * argv[])
{
    FILE * f;
    f=fopen("conftestval", "w");
    if (!f) exit(1);
    fprintf (f, "%s", PLATFORM_COMPILER_$1);
    return 0;
}
            ], [
                eval opal_cv_compiler_$1=`cat conftestval`;
            ], [
                eval opal_cv_compiler_$1=UNKNOWN
            ], [
                eval opal_cv_compiler_$1=UNKNOWN
            ])
            CPPFLAGS=$CPPFLAGS_orig
    ])
    AC_DEFINE_UNQUOTED([OPAL_BUILD_PLATFORM_COMPILER_$1], $opal_cv_compiler_[$1],
                       [The compiler $lower which OMPI was built with])
])dnl


AC_DEFUN([OPAL_CHECK_COMPILER_STRINGIFY], [
    lower=m4_tolower($1)
    AC_CACHE_CHECK([for compiler $lower], opal_cv_compiler_[$1],
    [
            CPPFLAGS_orig=$CPPFLAGS
            CPPFLAGS="-I${top_ompi_srcdir}/opal/include/opal $CPPFLAGS"
            AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include "opal_portable_platform.h"

int main (int argc, char * argv[])
{
    FILE * f;
    f=fopen("conftestval", "w");
    if (!f) exit(1);
    fprintf (f, "%s", _STRINGIFY(PLATFORM_COMPILER_$1));
    return 0;
}
            ], [
                eval opal_cv_compiler_$1=`cat conftestval`;
            ], [
                eval opal_cv_compiler_$1=UNKNOWN
            ], [
                eval opal_cv_compiler_$1=UNKNOWN
            ])
            CPPFLAGS=$CPPFLAGS_orig
    ])
    AC_DEFINE_UNQUOTED([OPAL_BUILD_PLATFORM_COMPILER_$1], $opal_cv_compiler_[$1],
                       [The compiler $lower which OMPI was built with])
])dnl
