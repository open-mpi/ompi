# -*- shell-script -*-
#
# Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Highest priority, as it's the default
AC_DEFUN([MCA_ompi_rte_orte_PRIORITY], [100])

# Force this component to compile in static-only mode
AC_DEFUN([MCA_ompi_rte_orte_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# If component was selected, $1 will be 1 and we should set the base header
AC_DEFUN([MCA_ompi_rte_orte_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [ompi_rte_base_include="orte/rte_orte.h"])
    AC_DEFINE_UNQUOTED([OMPI_RTE_ORTE], [$1],
        [Defined to 1 if the OMPI runtime component is ORTE])
    AM_CONDITIONAL([OMPI_RTE_ORTE], [test $1 = 1])
])dnl

# MCA_rte_orte_CONFIG([action-if-can-compile], 
#                     [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_rte_orte_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/rte/orte/Makefile])

    # This will need to get more complicated when we can build against
    # an external ORTE.
    AC_ARG_WITH([orte],
        AC_HELP_STRING([--with-orte],
                       [Use ORTE run-time environment (default: yes)]))
    AS_IF([test "$with_orte" != "no"],
          [$1],
          [AC_MSG_NOTICE([ORTE disabled by user])
           $2])
])
