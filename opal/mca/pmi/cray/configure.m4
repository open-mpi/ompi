# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pmi_cray_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmi_cray_CONFIG], [
    AC_CONFIG_FILES([opal/mca/pmi/cray/Makefile])

    AC_REQUIRE([OPAL_CHECK_UGNI])
    OPAL_CHECK_PMI([pmi_cray], [pmi_cray_good=1], [pmi_cray_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$pmi_cray_good" = 1 -a "$opal_check_ugni_happy" = "yes"],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pmi_cray_CPPFLAGS])
    AC_SUBST([pmi_cray_LDFLAGS])
    AC_SUBST([pmi_cray_LIBS])

])
