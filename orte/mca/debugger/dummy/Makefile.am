#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        dummy.h \
	dummy.c \
        dummy_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_debugger_dummy_DSO
component_noinst =
component_install = mca_debugger_dummy.la
else
component_noinst = libmca_debugger_dummy.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_debugger_dummy_la_SOURCES = $(sources)
mca_debugger_dummy_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_debugger_dummy_la_SOURCES =$(sources)
libmca_debugger_dummy_la_LDFLAGS = -module -avoid-version
