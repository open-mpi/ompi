# -*- shell-script -*-
#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$

# set our priority to be low (want to be after ORTE)
AC_DEFUN([MCA_ompi_rte_pmi_PRIORITY], [1])

# Force this component to compile in static-only mode
AC_DEFUN([MCA_ompi_rte_pmi_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# If component was selected, $1 will be 1 and we should set the base header
AC_DEFUN([MCA_ompi_rte_pmi_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [ompi_rte_base_include="pmi/rte_pmi.h"])
])dnl

#
# MCA_ompi_rte_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_rte_pmi_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/rte/pmi/Makefile])
         
    OPAL_CHECK_PMI([rte_pmi], [rte_pmi_good=1], [rte_pmi_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$rte_pmi_good" = 1],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([rte_pmi_CPPFLAGS])
    AC_SUBST([rte_pmi_LDFLAGS])
    AC_SUBST([rte_pmi_LIBS])
])
