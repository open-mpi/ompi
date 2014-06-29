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

    AC_REQUIRE([OPAL_CHECK_UGNI])
    OPAL_CHECK_PMI([pmix_cray], [pmix_cray_good=1], [pmix_cray_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$pmix_cray_good" = 1 -a "$opal_check_ugni_happy" = "yes"],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pmix_cray_CPPFLAGS])
    AC_SUBST([pmix_cray_LDFLAGS])
    AC_SUBST([pmix_cray_LIBS])

])
