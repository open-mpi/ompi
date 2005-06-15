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

libmca_bmi_self_la_sources = \
    bmi_self.c \
    bmi_self.h \
    bmi_self_component.c \
    bmi_self_frag.c \
    bmi_self_frag.h 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_bmi_self_DSO
component_noinst =
component_install = mca_bmi_self.la
else
component_noinst = libmca_bmi_self.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_bmi_self_la_SOURCES = $(libmca_bmi_self_la_sources)
mca_bmi_self_la_LDFLAGS = -module -avoid-version
mca_bmi_self_la_LIBADD = 

noinst_LTLIBRARIES = $(component_noinst)
libmca_bmi_self_la_SOURCES = $(libmca_bmi_self_la_sources)
libmca_bmi_self_la_LDFLAGS = -module -avoid-version
libmca_bmi_self_la_LIBADD = 
