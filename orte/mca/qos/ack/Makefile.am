#
#  Copyright (c) 2015      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

sources = \
	qos_ack.h \
	qos_ack_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_qos_ack_DSO
component_noinst =
component_install = mca_qos_ack.la
else
component_noinst = libmca_qos_ack.la
component_install =
endif

mcacomponentdir = $(ortelibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_qos_ack_la_SOURCES = $(sources)
mca_qos_ack_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_qos_ack_la_SOURCES = $(sources)
libmca_qos_ack_la_LDFLAGS = -module -avoid-version

