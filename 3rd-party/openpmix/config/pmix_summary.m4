dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2016-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2018-2023 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2021-2022 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl PMIX_SUMMARY_ADD(section, topic, unused, result)
dnl
dnl Turn around and call OAC_SUMMARY_ADD
dnl -----------------------------------------------------------
AC_DEFUN([PMIX_SUMMARY_ADD],[
    OAC_SUMMARY_ADD([$1], [$2], [$4])
])

dnl PMIX_SUMMARY_PRINT
dnl
dnl Print a bunch of PMIX summary configuration information and
dnl then turn around and call OAC_SUMMARY_PRINT.
dnl -----------------------------------------------------------
AC_DEFUN([PMIX_SUMMARY_PRINT],[
    cat <<EOF >&2

PMIx configuration:
-----------------------
Version: $PMIX_MAJOR_VERSION.$PMIX_MINOR_VERSION.$PMIX_RELEASE_VERSION$PMIX_GREEK_VERSION
PMIx Standard Version: $PMIX_STD_VERSION
PMIx Standard Stable ABI Version(s): $PMIX_STD_ABI_STABLE_VERSION
PMIx Standard Provisional ABI Version(s): $PMIX_STD_ABI_PROVISIONAL_VERSION
EOF

    if test $WANT_DEBUG = 0 ; then
        echo "Debug build: no" >&2
    else
        echo "Debug build: yes" >&2
    fi

    if test ! -z $with_pmix_platform ; then
        echo "Platform file: $with_pmix_platform" >&2
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
