# -*- shell-script -*-
#
# Copyright (c) 2010 	  Cisco Systems, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_errmgr_orcm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_errmgr_orcm_CONFIG],[
    # If we don't want FT, don't compile this component
    AS_IF([test "$ompi_want_ft" = "1"],
        [$1],
        [$2])
])dnl
