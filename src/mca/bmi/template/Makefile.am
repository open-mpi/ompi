#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
    bmi_template.c \
    bmi_template.h \
    bmi_template_component.c \
    bmi_template_endpoint.c \
    bmi_template_endpoint.h \
    bmi_template_frag.c \
    bmi_template_frag.h \
    bmi_template_proc.c \
    bmi_template_proc.h \
    bmi_template_error.h
     
# Make the output lgmrary in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or lgmmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_bmi_template_DSO
lgm =
lgm_sources =
component = mca_bmi_template.la
component_sources = $(sources)
else
lgm = lgmmca_bmi_template.la
lgm_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(lgmdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_bmi_template_la_SOURCES = $(component_sources)
mca_bmi_template_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lgm)
lgmmca_bmi_template_la_SOURCES = $(lgm_sources)
lgmmca_bmi_template_la_LDFLAGS = -module -avoid-version
