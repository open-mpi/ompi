dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2016-2023 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl PRTE_SUMMARY_ADD(section, topic, unused, result)
dnl
dnl Turn around and call OAC_SUMMARY_ADD
dnl -----------------------------------------------------------
AC_DEFUN([PRTE_SUMMARY_ADD],[
    OAC_SUMMARY_ADD([$1], [$2], [$4])
])

dnl PRTE_SUMMARY_PRINT
dnl
dnl Print a bunch of PRRTE summary configuration information and
dnl then turn around and call OAC_SUMMARY_PRINT.
dnl -----------------------------------------------------------
AC_DEFUN([PRTE_SUMMARY_PRINT],[
    cat <<EOF >&2

PRTE configuration:
-----------------------
Version: $PRTE_MAJOR_VERSION.$PRTE_MINOR_VERSION.$PRTE_RELEASE_VERSION$PRTE_GREEK_VERSION
EOF

    if test $WANT_DEBUG = 0 ; then
        echo "Debug build: no" >&2
    else
        echo "Debug build: yes" >&2
    fi

    if test ! -z $with_prte_platform ; then
        echo "Platform file: $with_prte_platform" >&2
    else
        echo "Platform file: (none)" >&2
    fi

    echo >&2

    OAC_SUMMARY_PRINT([stderr])

    if test $WANT_DEBUG = 1 ; then
        cat <<EOF >&2
*****************************************************************************
 THIS IS A DEBUG BUILD!  DO NOT USE THIS BUILD FOR PERFORMANCE MEASUREMENTS!
*****************************************************************************

EOF
    fi
])
