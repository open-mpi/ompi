# -*- autoconf -*-
#
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_coll_sm_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/sm/Makefile])

    OPAL_MCA_CHECK_DEPENDENCY([ompi], [coll], [sm], [opal], [common], [sm])

    $1
])dnl
