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
predefined_ompi_predefined_LDADD = \
       $(top_builddir)/ompi/libmpi.la \
       $(top_builddir)/orte/libopen-rte.la \
       $(top_builddir)/opal/libopen-pal.la 
predefined_ompi_predefined_DEPENDENCIES = $(ompi_predefined_LDADD)
