#
# Copyright (c) 2016      Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
          schizo_slurm_component.c \
          schizo_slurm.h \
          schizo_slurm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_schizo_slurm_DSO
component_noinst =
component_install = mca_schizo_slurm.la
else
component_noinst = libmca_schizo_slurm.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_schizo_slurm_la_SOURCES = $(sources)
mca_schizo_slurm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_schizo_slurm_la_SOURCES = $(sources)
libmca_schizo_slurm_la_LDFLAGS = -module -avoid-version

