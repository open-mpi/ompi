#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
        sbgp_basesmuma.h \
        sbgp_basesmuma_component.c  \
        sbgp_basesmuma_module.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_sbgp_basesmuma_DSO
component_install += mca_sbgp_basesmuma.la
else
component_noinst += libmca_sbgp_basesmuma.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sbgp_basesmuma_la_SOURCES = $(sources)
mca_sbgp_basesmuma_la_LDFLAGS = -module -avoid-version
mca_sbgp_basesmuma_la_LIBADD = 

noinst_LTLIBRARIES = $(component_noinst)
libmca_sbgp_basesmuma_la_SOURCES =$(sources)
libmca_sbgp_basesmuma_la_LDFLAGS = -module -avoid-version
