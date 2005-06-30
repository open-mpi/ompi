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
    bmi_ib.c \
    bmi_ib.h \
    bmi_ib_component.c \
    bmi_ib_endpoint.c \
    bmi_ib_endpoint.h \
    bmi_ib_frag.c \
    bmi_ib_frag.h \
    bmi_ib_proc.c \
    bmi_ib_proc.h \
    bmi_ib_error.h
     
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_bmi_ib_DSO
lib =
lib_sources =
component = mca_bmi_ib.la
component_sources = $(sources)
else
lib = libmca_bmi_ib.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component)
mca_bmi_ib_la_SOURCES = $(component_sources)
mca_bmi_ib_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(lib)
libmca_bmi_ib_la_SOURCES = $(lib_sources)
libmca_bmi_ib_la_LDFLAGS = -module -avoid-version
