# -*- shell-script -*-
#
# Copyright (c) 2009-2010 The Trustees of Indiana University.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_errmgr_crmig_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_errmgr_crmig_CONFIG],[
    # If we don't want FT, don't compile this component
    AS_IF([test "$opal_want_ft_cr" = "1"],
          [$1],
          [$2])
])dnl
