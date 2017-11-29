# -*- shell-script -*-
#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
#
# Copyright (c) 2017      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Higher priority to override the default
AC_DEFUN([MCA_ompi_rte_pmix_PRIORITY], [50])

# Force this component to compile in static-only mode
AC_DEFUN([MCA_ompi_rte_pmix_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# If component was selected, $1 will be 1 and we should set the base header
AC_DEFUN([MCA_ompi_rte_pmix_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [ompi_rte_base_include="pmix/rte_pmix.h"])
    AC_DEFINE_UNQUOTED([OMPI_RTE_PMIX], [$1],
        [Defined to 1 if the OMPI runtime component is PMIX])
    AM_CONDITIONAL([OMPI_RTE_PMIX], [test $1 = 1])
])dnl

# MCA_rte_pmix_CONFIG([action-if-can-compile],
#                     [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_rte_pmix_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/rte/pmix/Makefile])

    AC_ARG_WITH([ompi-pmix-rte],
        AC_HELP_STRING([--with-ompi-pmix-rte],
                       [Use PMIx as the OMPI run-time environment (default: no)]))
    AS_IF([test "$with_ompi_pmix_rte" == "yes"],
          [$1
           AC_MSG_NOTICE([PMIx RTE selected by user])],
          [$2])
])
