#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers += \
        base/base.h \
        base/rmaps_private.h

libmca_rmaps_la_SOURCES += \
        base/rmaps_base_frame.c \
        base/rmaps_base_select.c \
        base/rmaps_base_map_job.c \
        base/rmaps_base_support_fns.c \
        base/rmaps_base_ranking.c \
        base/rmaps_base_print_fns.c

dist_ompidata_DATA = base/help-orte-rmaps-base.txt

if OPAL_HAVE_HWLOC
libmca_rmaps_la_SOURCES += \
        base/rmaps_base_binding.c
endif
