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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

amca_paramdir = $(AMCA_PARAM_SETS_DIR)
dist_amca_param_DATA = netpipe-btl-template.txt

sources = \
    btl_template.c \
    btl_template.h \
    btl_template_component.c \
    btl_template_endpoint.c \
    btl_template_endpoint.h \
    btl_template_frag.c \
    btl_template_frag.h \
    btl_template_proc.c \
    btl_template_proc.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_btl_template_DSO
lib =
lib_sources =
component = mca_btl_template.la
component_sources = $(sources)
else
lib = libmca_btl_template.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_btl_template_la_SOURCES = $(component_sources)
mca_btl_template_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_btl_template_la_SOURCES = $(lib_sources)
libmca_btl_template_la_LDFLAGS = -module -avoid-version
