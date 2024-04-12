# -*- shell-script ; indent-tabs-mode:nil -*-
#
# Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# PRTE_CHECK_SINGULARITY()
# --------------------------------------------------------
AC_DEFUN([PRTE_CHECK_SINGULARITY],[
    PRTE_VAR_SCOPE_PUSH([spath have_singularity])

    AC_ARG_WITH([singularity],
                [AS_HELP_STRING([--with-singularity(=DIR)],
                                [Build support for the Singularity container, optionally adding DIR to the search path])])
    spath=
    AC_MSG_CHECKING([if Singularity is present])
    AS_IF([test "$with_singularity" = "no"],
          [AC_MSG_RESULT([no])],
          [AC_MSG_RESULT([yes])
           AS_IF([test -z "$with_singularity" || test "$with_singularity" = "yes"],
                 [ # look for the singularity command in the default path
                   AC_CHECK_PROG([SINGULARITY], [singularity], [singularity])
                   AS_IF([test "$SINGULARITY" != ""],
                         [spath=DEFAULT],
                         [AS_IF([test "$with_singularity" = "yes"],
                                [AC_MSG_WARN([Singularity support requested, but required executable])
                                 AC_MSG_WARN(["singularity" not found in default locations])
                                 AC_MSG_ERROR([Cannot continue])])])],
                 [ AC_MSG_CHECKING([for existence of $with_singularity/bin])
                   # look for the singularity command in the bin subdirectory
                   AS_IF([test ! -d "$with_singularity/bin"],
                         [AC_MSG_RESULT([not found])
                          AC_MSG_WARN([Directory $with_singularity/bin not found])
                          AC_MSG_ERROR([Cannot continue])],
                         [AC_MSG_RESULT([found])])
                   save_path=$PATH
                   PATH=$with_singularity/bin:$PATH
                   AC_CHECK_PROG([SINGULARITY], [singularity], [singularity])
                   AS_IF([test "$SINGULARITY" != ""],
                         [spath=$with_singularity/bin],
                         [AC_MSG_WARN([Singularity support requested, but required executable])
                          AC_MSG_WARN(["singularity" not found in either default or specified path])
                          AC_MSG_ERROR([Cannot continue])])
                   PATH=$save_path
                 ]
           )])

    AC_DEFINE_UNQUOTED(PRTE_SINGULARITY_PATH, "$spath", [Path to Singularity binaries])

    PRTE_VAR_SCOPE_POP
])
