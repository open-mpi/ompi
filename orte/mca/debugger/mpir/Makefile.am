#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        mpir.h \
	mpir.c \
        mpir_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_debugger_mpir_DSO
component_noinst =
component_install = mca_debugger_mpir.la
else
component_noinst = libmca_debugger_mpir.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_debugger_mpir_la_SOURCES = $(sources)
mca_debugger_mpir_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_debugger_mpir_la_SOURCES =$(sources)
libmca_debugger_mpir_la_LDFLAGS = -module -avoid-version
