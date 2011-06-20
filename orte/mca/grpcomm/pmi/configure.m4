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
         
    # Evaluate succeed / fail
    AS_IF([test "$orte_enable_slurm_pmi" = "1"],
          [$1],
          [$2])


])
