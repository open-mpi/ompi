# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from ompi/Makefile.am

EXTRA_DIST += tools/win_makefile tools/CMakeLists.txt

SUBDIRS += \
	tools/wrappers

DIST_SUBDIRS += \
	tools/wrappers

# Don`t build this tool during OMPI build
# it is built as part of OSHMEM build
if OSHMEM_SUPPORT
else
   SUBDIRS += \
	tools/ompi_info
   DIST_SUBDIRS += \
	tools/ompi_info
endif

