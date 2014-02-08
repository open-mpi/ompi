#
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
EXTRA_DIST = .windows
not_used_yet = 

sources = \
        sbgp_basesmsocket.h \
        sbgp_basesmsocket_component.c  \
        sbgp_basesmsocket_module.c


# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

component_noinst =
component_install =
if MCA_BUILD_ompi_sbgp_basesmsocket_DSO
component_install += mca_sbgp_basesmsocket.la
else
component_noinst += libmca_sbgp_basesmsocket.la
endif

# See ompi/mca/btl/sm/Makefile.am for an explanation of
# libmca_common_sm.la.

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sbgp_basesmsocket_la_SOURCES = $(sources)
mca_sbgp_basesmsocket_la_LDFLAGS = -module -avoid-version
mca_sbgp_basesmsocket_la_LIBADD = 

noinst_LTLIBRARIES = $(component_noinst)
libmca_sbgp_basesmsocket_la_SOURCES =$(sources)
libmca_sbgp_basesmsocket_la_LDFLAGS = -module -avoid-version
