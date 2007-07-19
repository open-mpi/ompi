# -*- makefile -*-
#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from orte/Makefile.am

SUBDIRS += \
	tools/orteboot \
	tools/orted \
	tools/ortehalt \
	tools/ortekill \
	tools/orteprobe \
	tools/orterun \
	tools/wrappers \
	tools/orte-ps \
	tools/orte-clean

DIST_SUBDIRS += \
	tools/orteboot \
	tools/orted \
	tools/ortehalt \
	tools/ortekill \
	tools/orteprobe \
	tools/orterun \
	tools/wrappers \
	tools/orte-ps \
	tools/orte-clean \
	tools/orte-checkpoint \
	tools/orte-restart

if WANT_FT
SUBDIRS += \
	tools/orte-checkpoint \
	tools/orte-restart
endif
