# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_cray_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_cray_CONFIG], [
    AC_CONFIG_FILES([opal/mca/pmix/cray/Makefile])

#
#   check specifically for Cray PMI here
#
    OPAL_CHECK_CRAY_PMI([pmix_cray], [pmix_cray_good=1], [pmix_cray_good=0])

    # Evaluate succeed / fail
    AS_IF([test "$pmix_cray_good" = 1],
          [$1
           # need to set the wrapper flags for static builds
           pmix_cray_WRAPPER_EXTRA_LDFLAGS="$pmix_cray_LDFLAGS"
           pmix_cray_WRAPPER_EXTRA_LIBS="$pmix_cray_LIBS"],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pmix_cray_CPPFLAGS])
    AC_SUBST([pmix_cray_LDFLAGS])
    AC_SUBST([pmix_cray_LIBS])

])
