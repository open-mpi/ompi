#
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        sec_keystone.h \
        sec_keystone_component.c \
        sec_keystone.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_sec_keystone_DSO
component_noinst =
component_install = mca_sec_keystone.la
else
component_noinst = libmca_sec_keystone.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sec_keystone_la_SOURCES = $(sources)
mca_sec_keystone_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_sec_keystone_la_SOURCES =$(sources)
libmca_sec_keystone_la_LDFLAGS = -module -avoid-version
