# -*- shell-script -*-
#
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2014-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Priority
#
AC_DEFUN([MCA_opal_pmix_external_PRIORITY], [90])

#
# Force this component to compile in static-only mode
#
AC_DEFUN([MCA_opal_pmix_external_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_pmix_external_POST_CONFIG()
# ---------------------------------
AC_DEFUN([MCA_opal_pmix_external_POST_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_pmix_external_basedir])

    # If we won, then do all the rest of the setup
    AS_IF([test "$1" = "1"],
          [ # Set this variable so that the framework m4 knows what
            # file to include in opal/mca/pmix/pmix-internal.h
            # The CPPFLAGS, LDFLAGS, and LIBS were already set
            # by the configury
           opal_pmix_external_basedir=opal/mca/pmix/external
           opal_pmix_base_include="$opal_pmix_external_basedir/external.h"
          ])
    OPAL_VAR_SCOPE_POP
])dnl


# MCA_pmix_external_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_external_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/external/Makefile])

    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [$1], [$2])

    OPAL_VAR_SCOPE_POP
])dnl
