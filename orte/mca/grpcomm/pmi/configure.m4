# -*- shell-script -*-
#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_orte_grpcomm_pmi_PRIORITY], [10])

# MCA_grpcomm_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_grpcomm_pmi_CONFIG], [
    AC_CONFIG_FILES([orte/mca/grpcomm/pmi/Makefile])
         
    ORTE_CHECK_SLURM([grpcomm_pmi], [grpcomm_pmi_good=1], [grpcomm_pmi_good=0])
         
    # if check worked, set wrapper flags if so.  
    # Evaluate succeed / fail
    AS_IF([test "$grpcomm_pmi_good" = 1 -a "$orte_enable_slurm_pmi" = "1"],
          [grpcomm_pmi_WRAPPER_EXTRA_LDFLAGS="$grpcomm_pmi_LDFLAGS"
           grpcomm_pmiWRAPPER_EXTRA_LIBS="$grpcomm_pmi_LIBS"
           $1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([grpcomm_pmi_CPPFLAGS])
    AC_SUBST([grpcomm_pmi_LDFLAGS])
    AC_SUBST([grpcomm_pmi_LIBS])

])
