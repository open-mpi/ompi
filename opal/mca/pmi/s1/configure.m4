# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pmi_s1_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmi_s1_CONFIG], [
    AC_CONFIG_FILES([opal/mca/pmi/s1/Makefile])
         
    AC_REQUIRE([OPAL_CHECK_UGNI])
    OPAL_CHECK_PMI([pmi_s1], [pmi_s1_good=1], [pmi_s1_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$pmi_s1_good" = 1 -a "$opal_check_ugni_happy" = "no"],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pmi_s1_CPPFLAGS])
    AC_SUBST([pmi_s1_LDFLAGS])
    AC_SUBST([pmi_s1_LIBS])

])
