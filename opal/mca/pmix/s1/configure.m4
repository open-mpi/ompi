# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_s1_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_s1_CONFIG], [
    AC_CONFIG_FILES([opal/mca/pmix/s1/Makefile])

    AC_REQUIRE([OPAL_CHECK_UGNI])
    AC_REQUIRE([OPAL_CHECK_PMI])

    # Evaluate succeed / fail
    AS_IF([test "$opal_enable_pmi1" = "yes" && test "$opal_check_ugni_happy" = "no"],
          [$1
           # need to set the wrapper flags for static builds
           pmix_s1_WRAPPER_EXTRA_LDFLAGS="$opal_pmi1_LDFLAGS"
           pmix_s1_WRAPPER_EXTRA_LIBS="$opal_pmi1_LIBS"],
          [$2])

])
