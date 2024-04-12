#
# Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CFLAGS = \
            -DDEFAULT_PARAM_FILE_PATH="\"@AMCA_PARAM_SETS_DIR@\""

# We install the "source" RST file so that Open MPI can find it and
# use it when building against an external PRTE installation.
#
# See src/docs/included-content/Makefile.am for an explanation why we
# install this file into the "data" directory (vs. the "doc"
# directory).
#
# NOTE: This file is a ReStructured Text file (RST), but we
# intentionally name it .rstxt.  See the comment in the Open MPI
# docs/Makefile.am file for an explanation why.
rstdir = $(prtedatadir)/rst/schizo-ompi-rst-content
dist_rst_DATA = \
        schizo-ompi-cli.rstxt

# Install the help file
dist_prtedata_DATA = \
        help-schizo-ompi.txt

sources = \
        schizo_ompi_component.c \
        schizo_ompi.h \
        schizo_ompi.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_prte_schizo_ompi_DSO
component_noinst =
component_install = prte_mca_schizo_ompi.la
else
component_noinst = libprtemca_schizo_ompi.la
component_install =
endif

mcacomponentdir = $(prtelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
prte_mca_schizo_ompi_la_SOURCES = $(sources)
prte_mca_schizo_ompi_la_LDFLAGS = -module -avoid-version
prte_mca_schizo_ompi_la_LIBADD = $(top_builddir)/src/libprrte.la

noinst_LTLIBRARIES = $(component_noinst)
libprtemca_schizo_ompi_la_SOURCES = $(sources)
libprtemca_schizo_ompi_la_LDFLAGS = -module -avoid-version
