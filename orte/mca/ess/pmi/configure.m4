# -*- shell-script -*-
#
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ess_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_pmi_CONFIG],[
    AC_CONFIG_FILES([orte/mca/ess/pmi/Makefile])

    # see if PMI support requested
    OPAL_CHECK_PMI([ess_pmi], [ess_pmi_good=1], [ess_pmi_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$ess_pmi_good" = "1"],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([ess_pmi_CPPFLAGS])
    AC_SUBST([ess_pmi_LDFLAGS])
    AC_SUBST([ess_pmi_LIBS])

])dnl
