# -*- shell-script -*-
#
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2018 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OSHMEM_CONFIG_FILES],[
    AC_CONFIG_FILES([
    oshmem/Makefile
    oshmem/include/Makefile
    oshmem/shmem/c/Makefile

    oshmem/shmem/fortran/Makefile

    oshmem/tools/oshmem_info/Makefile
    oshmem/tools/wrappers/Makefile
    oshmem/tools/wrappers/shmemcc-wrapper-data.txt
    oshmem/tools/wrappers/shmemc++-wrapper-data.txt
    oshmem/tools/wrappers/shmemfort-wrapper-data.txt
    oshmem/tools/wrappers/oshmem.pc
    oshmem/tools/wrappers/oshmem-c.pc
    oshmem/tools/wrappers/oshmem-cxx.pc
    oshmem/tools/wrappers/oshmem-fort.pc
    ])
])
