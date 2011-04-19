# -*- shell-script -*-
#
# Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_btl_wv_CONFIG([action-if-can-compile], 
#                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_btl_wv_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/wv/Makefile])

    # AFAIK, cmake will only ever be used to build this component, so
    # the autogen version can always simply fail.
    $2
])dnl
