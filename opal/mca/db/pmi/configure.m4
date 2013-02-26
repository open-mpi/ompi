# -*- shell-script -*-
#
# Copyright (c) 2012-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_db_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_db_pmi_CONFIG], [
    AC_CONFIG_FILES([opal/mca/db/pmi/Makefile])
         
    OPAL_CHECK_PMI([db_pmi], [db_pmi_good=1], [db_pmi_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$db_pmi_good" = 1],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([db_pmi_CPPFLAGS])
    AC_SUBST([db_pmi_LDFLAGS])
    AC_SUBST([db_pmi_LIBS])

])
