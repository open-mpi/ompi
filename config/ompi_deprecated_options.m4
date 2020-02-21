# -*- shell-script -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_CHECK_DEPRECATED_OPTIONS],[
    OPAL_VAR_SCOPE_PUSH([enable_orterun_prefix_given])

    # --enable-orterun-prefix-by-default was deprecated in v5 in favor of --enable-prte-prefix-by-default
    AC_ARG_ENABLE([orterun-prefix-by-default],
        [AC_HELP_STRING([--enable-orterun-prefix-by-default],
            [*DEPRECATED* Please use --enable-prte-prefix-by-default in the future)])],
        [enable_orterun_prefix_given=yes])
    AC_ARG_ENABLE([mpirun-prefix-by-default],
        [AC_HELP_STRING([--enable-mpirun-prefix-by-default],
            [*DEPRECATED* Please use --enable-prte-prefix-by-default in the future])],
        [enable_orterun_prefix_given=yes])

    if test "$enable_orterun_prefix_given" = "yes"; then
        AC_MSG_WARN([Open MPI no longer uses the ORTE environment - it has been])
        AC_MSG_WARN([replaced by PRRTE. Accordingly, the "--enable-orterun-prefix-by-default"])
        AC_MSG_WARN([and "--enable-mpirun-prefix-by-default" options have been replaced])
        AC_MSG_WARN([by "--enable-prte-prefix-by-default". We will do the translation for])
        AC_MSG_WARN([you now, but these older options are deprecated and will be removed])
        AC_MSG_WARN([in a later release, so please update your build scripts.])
    fi

    OPAL_VAR_SCOPE_POP
])
