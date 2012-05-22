# -*- shell-script -*-
#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2012 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_common_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_common_pmi_CONFIG], [
    AC_CONFIG_FILES([orte/mca/common/pmi/Makefile])
         
    ORTE_CHECK_PMI([common_pmi], [common_pmi_good=1], [common_pmi_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$common_pmi_good" = 1 -a "$orte_without_full_support" = 0],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([common_pmi_CPPFLAGS])
    AC_SUBST([common_pmi_LDFLAGS])
    AC_SUBST([common_pmi_LIBS])

])
