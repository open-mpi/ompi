# -*- makefile -*-
#
# Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

check_PROGRAMS = \
	predefined/ompi_predefined

predefined_ompi_predefined_SOURCES = predefined/ompi_predefined.c

# We don't even need to link against any of the OMPI libraries.  How
# fun.
