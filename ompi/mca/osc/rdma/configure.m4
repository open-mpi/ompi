# -*- shell-script -*-
#
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ompi_osc_rdma_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built, since bml
# is one of the ones frequently disabled for large installs.
AC_DEFUN([MCA_ompi_osc_rdma_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([BML])])
])dnl

# MCA_ompi_osc_rdma_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
# We can always build, unless we were explicitly disabled.
AC_DEFUN([MCA_ompi_osc_rdma_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/osc/rdma/Makefile])
    [$1]
])dnl
