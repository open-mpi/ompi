#
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_hook_CONFIG],[
    # configure all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)
])
