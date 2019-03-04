# -*- shell-script -*-
#
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_s2_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_s2_CONFIG], [
    AC_CONFIG_FILES([opal/mca/pmix/s2/Makefile])

    AC_REQUIRE([OPAL_CHECK_UGNI])

    # Evaluate succeed / fail
    AS_IF([test "$opal_enable_pmi2" = "yes" && test "$opal_check_ugni_happy" = "no"],
          [$1
           # need to set the wrapper flags for static builds
           pmix_s2_WRAPPER_EXTRA_LDFLAGS="$opal_pmi2_LDFLAGS"
           pmix_s2_WRAPPER_EXTRA_LIBS="$opal_pmi2_LIBS"],
          [$2])

])
