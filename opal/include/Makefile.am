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
# Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

headers = \
	opal_config_top.h \
	opal_config_bottom.h \
	opal_stdint.h

nodist_headers = opal_config.h

include opal/Makefile.am

EXTRA_DIST = CMakeLists.txt $(headers)

if WANT_INSTALL_HEADERS 
opaldir = $(ompiincludedir)
nobase_dist_opal_HEADERS = $(headers)
nobase_nodist_opal_HEADERS = $(nodist_headers)
endif

