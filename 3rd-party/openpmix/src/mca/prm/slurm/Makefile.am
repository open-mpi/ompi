#
# Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
          prm_slurm_component.c \
          prm_slurm.h \
          prm_slurm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_pmix_prm_slurm_DSO
component_noinst =
component_install = pmix_mca_prm_slurm.la
else
component_noinst = libpmix_mca_prm_slurm.la
component_install =
endif

mcacomponentdir = $(pmixlibdir)
mcacomponent_LTLIBRARIES = $(component_install)
pmix_mca_prm_slurm_la_SOURCES = $(sources)
pmix_mca_prm_slurm_la_LDFLAGS = -module -avoid-version
if NEED_LIBPMIX
pmix_mca_prm_slurm_la_LIBADD = $(top_builddir)/src/libpmix.la
endif

noinst_LTLIBRARIES = $(component_noinst)
libpmix_mca_prm_slurm_la_SOURCES = $(sources)
libpmix_mca_prm_slurm_la_LDFLAGS = -module -avoid-version
