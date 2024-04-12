# -*- shell-script ; indent-tabs-mode:nil -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dnl $1 is the base cap name (i.e., what comes after "PMIX_CAP_")
dnl $2 is the action if happy
dnl $3 is the action if not happy
AC_DEFUN([PRTE_CHECK_PMIX_CAP],[
    PRTE_VAR_SCOPE_PUSH([prte_cpp_save])

    AC_MSG_CHECKING([for PMIX_CAP_$1])

    prte_cpp_save=$CPP
    CPP="$PMIXCC_PATH -E"
    AC_PREPROC_IFELSE(
        [AC_LANG_PROGRAM([#include <pmix_version.h>],
                         [#if !defined(PMIX_CAPABILITIES)
                          #error This PMIx does not have any capability flags
                          #endif
                          #if !defined(PMIX_CAP_$1)
                          #error This PMIx does not have the PMIX_CAP_$1 capability flag at all
                          #endif
                          #if (PMIX_CAPABILITIES & PMIX_CAP_$1) == 0
                          #error This PMIx does not have the PMIX_CAP_$1 capability flag set
                          #endif
                         ]
                        )
        ],
        [AC_MSG_RESULT([found])
         $2],
        [AC_MSG_RESULT([not found])
         $3])

    CPP=$prte_cpp_save

    PRTE_VAR_SCOPE_POP
])

AC_DEFUN([PRTE_CHECK_PMIX],[

    PRTE_VAR_SCOPE_PUSH([prte_external_pmix_save_CPPFLAGS prte_pmix_support found_pmixcc])

    AC_ARG_WITH([pmix],
                [AS_HELP_STRING([--with-pmix(=DIR)],
                                [Where to find PMIx support, optionally adding DIR to the search path])])
    AC_ARG_WITH([pmix-libdir],
                [AS_HELP_STRING([--with-pmix-libdir=DIR],
                                [Look for libpmix in the given directory DIR, DIR/lib or DIR/lib64])])
    AC_ARG_WITH([pmix-extra-libs],
                [AS_HELP_STRING([--with-pmix-extra-libs=LIBS],
                                [Add LIBS as dependencies of pmix])])
    AC_ARG_ENABLE([pmix-lib-checks],
                  [AS_HELP_STRING([--disable-pmix-lib-checks],
                                  [If --disable-pmix-lib-checks is specified, configure will assume that -lpmix is available])])

    prte_pmix_support=1

    if test "$with_pmix" = "no"; then
        AC_MSG_WARN([PRTE requires PMIx support using])
        AC_MSG_WARN([an external copy that you supply.])
        AC_MSG_ERROR([Cannot continue])
    fi

    AS_IF([test "$with_pmix_extra_libs" = "yes" -o "$with_pmix_extra_libs" = "no"],
	  [AC_MSG_ERROR([--with-pmix-extra-libs requires an argument other than yes or no])])

    AS_IF([test "$enable_pmix_lib_checks" != "no"],
          [dnl Need to explicitly enable wrapper compiler to get the dependent libraries
           dnl when pkg-config is not available.
           pmix_USE_WRAPPER_COMPILER=1
           OAC_CHECK_PACKAGE([pmix],
                             [prte_pmix],
                             [pmix.h],
                             [pmix $with_pmix_extra_libs],
                             [PMIx_Init],
                             [],
                             [prte_pmix_support=0])],
          [PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LIBS], [$with_pmix_extra_libs])])

    AS_IF([test $prte_pmix_support -eq 0],
          [AC_MSG_WARN([PRRTE requires PMIx support using an external copy that you supply.])
           AC_MSG_ERROR([Cannot continue.])])

    prte_external_pmix_save_CPPFLAGS=$CPPFLAGS
    PRTE_FLAGS_PREPEND_UNIQ(CPPFLAGS, $prte_pmix_CPPFLAGS)

    # if the version file exists, then we need to parse it to find
    # the actual release series
    # NOTE: We have already read PRRTE's VERSION file, so we can use
    # $pmix_min_version.
    prte_pmix_min_num_version=PRTE_PMIX_NUMERIC_MIN_VERSION
    prte_pmix_min_version=PRTE_PMIX_MIN_VERSION
    AC_MSG_CHECKING([version at or above v$prte_pmix_min_version])
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                        #include <pmix_version.h>
                                        #if (PMIX_NUMERIC_VERSION < $prte_pmix_min_num_version)
                                        #error "not version $prte_pmix_min_num_version or above"
                                        #endif
                                       ], [])],
                      [AC_MSG_RESULT([yes])],
                      [AC_MSG_RESULT(no)
                       AC_MSG_WARN([PRRTE requires PMIx v$prte_pmix_min_num_version or above.])
                       AC_MSG_ERROR([Please select a supported version and configure again])])

    AC_CHECK_HEADER([src/util/pmix_argv.h], [],
                    [AC_MSG_ERROR([Could not find PMIx devel headers.  Can not continue.])])

    # restore the global flags
    CPPFLAGS=$prte_external_pmix_save_CPPFLAGS

    PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_CPPFLAGS, $prte_pmix_CPPFLAGS)
    PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LDFLAGS, $prte_pmix_LDFLAGS)
    PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LIBS, $prte_pmix_LIBS)

    AC_DEFINE_UNQUOTED([PRTE_PMIX_MINIMUM_VERSION],
                       [$prte_pmix_min_num_version],
                       [Minimum supported PMIx version])

    found_pmixcc=0
    PMIXCC_PATH="pmixcc"
    AS_IF([test -n "${with_pmix}"],
          [PMIXCC_PATH="${with_pmix}/bin/$PMIXCC_PATH"])
    PRTE_LOG_COMMAND([pmixcc_showme_results=`$PMIXCC_PATH --showme:version 2>&1`], [found_pmixcc=1])
    PRTE_LOG_MSG([pmixcc version: $pmixcc_showme_results])
    AS_IF([test $found_pmixcc -eq 0],
          [AC_MSG_WARN([Could not find $PMIXCC_PATH])
           PMIXCC_PATH=])
    AM_CONDITIONAL([PRTE_HAVE_PMIXCC], [test $found_pmixcc -eq 1])
    AC_SUBST([PMIXCC_PATH])

    # Check for any needed capabilities from the PMIx we found.
    #
    # Note: if the PMIx we found does not define capability flags,
    # then it definitely does not have the capability flags we're
    # looking for.

    # For now, we just check for the "base" capability to exercise
    # this feature - essentially retaining this as an example for
    # future times when we actually need to check capabilities
    PRTE_CHECK_PMIX_CAP([BASE],
                        [PRTE_PMIX_BASE_CAPABILITY=1],
                        [AC_MSG_WARN([Your PMIx version is either does not])
                         AC_MSG_WARN([the capabilities feature or does not])
                         AC_MSG_WARN([include the PMIX_CAP_BASE capability flag])
                         AC_MSG_WARN([Ignoring this for now])
                         PRTE_PMIX_BASE_CAPABILITY=0])

    AC_DEFINE_UNQUOTED([PRTE_PMIX_BASE_CAPABILITY],
                       [$PRTE_PMIX_BASE_CAPABILITY],
                       [Whether or not PMIx has the BASE capability flag set])

    PRTE_SUMMARY_ADD([Required Packages], [PMIx], [], [$prte_pmix_SUMMARY])

    PRTE_VAR_SCOPE_POP
])
