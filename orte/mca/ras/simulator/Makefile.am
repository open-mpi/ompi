#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_ortedata_DATA = help-ras-simulator.txt

sources = \
        ras_sim.h \
        ras_sim_component.c \
        ras_sim_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_ras_simulator_DSO
lib =
lib_sources =
component = mca_ras_simulator.la
component_sources = $(sources)
else
lib = libmca_ras_simulator.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component)
mca_ras_simulator_la_SOURCES = $(component_sources)
mca_ras_simulator_la_LDFLAGS = -module -avoid-version
mca_ras_simulator_la_LIBADD = $(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la

noinst_LTLIBRARIES = $(lib)
libmca_ras_simulator_la_SOURCES = $(lib_sources)
libmca_ras_simulator_la_LDFLAGS = -module -avoid-version
