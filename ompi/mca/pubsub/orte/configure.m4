# -*- shell-script -*-
#
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_pubsub_orte_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_pubsub_orte_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/pubsub/orte/Makefile])

    AC_ARG_WITH([orte],
        AC_HELP_STRING([--with-orte],
                       [Use ORTE run-time environment (default: yes)]))
    AS_IF([test "$with_orte" != "no"],
          [$1],
          [$2])
])
