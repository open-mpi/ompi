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
# MCA_dstore_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_dstore_pmi_CONFIG], [
    AC_CONFIG_FILES([opal/mca/dstore/pmi/Makefile])
         
    OPAL_CHECK_PMI([dstore_pmi], [dstore_pmi_good=1], [dstore_pmi_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$dstore_pmi_good" = 1],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([dstore_pmi_CPPFLAGS])
    AC_SUBST([dstore_pmi_LDFLAGS])
    AC_SUBST([dstore_pmi_LIBS])

])
