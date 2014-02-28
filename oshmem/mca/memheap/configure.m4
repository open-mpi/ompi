# -*- shell-script -*-
#
# Copyright (c) 2014      Mellanox Technologies, Inc.
#                         All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_oshmem_memheap_CONFIG],[
    # configure all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    # this is a direct callable component, so set that up.
    MCA_SETUP_DIRECT_CALL($1, $2)
])
