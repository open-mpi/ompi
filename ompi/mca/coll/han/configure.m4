# -*- autoconf -*-
#
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_coll_han_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/han/Makefile])
    $1
])dnl

AC_DEFUN([MCA_ompi_coll_han_POST_CONFIG], [
    OMPI_REQUIRE_ENDPOINT_TAG([SMSC])
])dnl
