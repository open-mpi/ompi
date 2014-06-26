#
# Copyright (c) 2014      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        pmi_cray.h \
        pmi_cray_component.c \
        pmi_cray.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmi_cray_DSO
component_noinst =
component_install = mca_pmi_cray.la
else
component_noinst = libmca_pmi_cray.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmi_cray_la_SOURCES = $(sources)
mca_pmi_cray_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmi_cray_la_SOURCES =$(sources)
libmca_pmi_cray_la_LDFLAGS = -module -avoid-version
