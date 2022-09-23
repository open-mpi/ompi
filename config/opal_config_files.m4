# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2020      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# This file is m4_included in the top-level configure.ac

AC_DEFUN([OPAL_CONFIG_FILES],[
    AC_CONFIG_FILES([
        opal/Makefile
        opal/cuda/Makefile
        opal/etc/Makefile
        opal/include/Makefile
        opal/datatype/Makefile
        opal/util/Makefile
        opal/util/keyval/Makefile
        opal/mca/base/Makefile
        opal/tools/wrappers/Makefile
        opal/tools/wrappers/opalcc-wrapper-data.txt
        opal/tools/wrappers/opal.pc
    ])
])
