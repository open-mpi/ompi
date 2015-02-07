#
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        sec_munge.h \
        sec_munge_component.c \
        sec_munge.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_sec_munge_DSO
component_noinst =
component_install = mca_sec_munge.la
else
component_noinst = libmca_sec_munge.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_sec_munge_la_SOURCES = $(sources)
mca_sec_munge_la_CPPFLAGS = $(sec_munge_CPPFLAGS)
mca_sec_munge_la_LDFLAGS = -module -avoid-version $(sec_munge_LDFLAGS)
mca_sec_munge_la_LIBADD = $(sec_munge_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_sec_munge_la_SOURCES =$(sources)
libmca_sec_munge_la_CPPFLAGS = $(sec_munge_CPPFLAGS)
libmca_sec_munge_la_LDFLAGS = -module -avoid-version $(sec_munge_LDFLAGS)
libmca_sec_munge_la_LIBADD = $(sec_munge_LIBS)
