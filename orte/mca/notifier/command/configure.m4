# -*- command-script -*-
#
# Copyright (c) 2007      Sandia National Laboratories. All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_notifier_command_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_notifier_command_CONFIG], [
    AC_CONFIG_FILES([orte/mca/notifier/command/Makefile])

    OMPI_VAR_SCOPE_PUSH(notifier_happy)

    notifier_happy=no

    # We need fork() and pipe()
    AC_CHECK_FUNC([fork], 
                  [AC_CHECK_FUNC([pipe], [notifier_happy=yes])])

    # We also need thread support
    AS_IF([test "$notifier_happy" = "yes"],
          [AC_MSG_CHECKING([for thread support])
           AC_MSG_RESULT([$THREAD_TYPE])
           AS_IF([test "$THREAD_TYPE" != "none"],
                 [notifier_happy=yes])])

    AS_IF([test "$notifier_happy" = "yes"], [$1], [$2])
    OMPI_VAR_SCOPE_POP
])
