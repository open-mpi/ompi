# -*- shell-script -*-
#
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
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
    oshmem/shmem/f77/Makefile

    oshmem/shmem/c/profile/Makefile

    oshmem/tools/wrappers/Makefile
    oshmem/tools/wrappers/shmemcc-wrapper-data.txt
    oshmem/tools/wrappers/shmemf77-wrapper-data.txt
    oshmem/tools/wrappers/shmemf90-wrapper-data.txt
    ])
])
