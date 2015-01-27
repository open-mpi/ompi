#
# Copyright (c) 2015      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
          schizo_ompi_component.c \
          schizo_ompi.h \
          schizo_ompi.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_schizo_ompi_DSO
component_noinst =
component_install = mca_schizo_ompi.la
else
component_noinst = libmca_schizo_ompi.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_schizo_ompi_la_SOURCES = $(sources)
mca_schizo_ompi_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_schizo_ompi_la_SOURCES = $(sources)
libmca_schizo_ompi_la_LDFLAGS = -module -avoid-version

