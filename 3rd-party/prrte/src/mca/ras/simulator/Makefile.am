#
# Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_prtedata_DATA = help-ras-simulator.txt

sources = \
        ras_sim.h \
        ras_sim_component.c \
        ras_sim_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_ras_simulator_DSO
lib =
lib_sources =
component = prte_mca_ras_simulator.la
component_sources = $(sources)
else
lib = libprtemca_ras_simulator.la
lib_sources = $(sources)
component =
component_sources =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component)
prte_mca_ras_simulator_la_SOURCES = $(component_sources)
prte_mca_ras_simulator_la_LDFLAGS = -module -avoid-version
prte_mca_ras_simulator_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(lib)
libprtemca_ras_simulator_la_SOURCES = $(lib_sources)
libprtemca_ras_simulator_la_LDFLAGS = -module -avoid-version
