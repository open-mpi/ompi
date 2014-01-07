#
# Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        db_print.h \
        db_print_component.c \
        db_print.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_db_print_DSO
component_noinst =
component_install = mca_db_print.la
else
component_noinst = libmca_db_print.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_print_la_CPPFLAGS = $(db_print_CPPFLAGS)
mca_db_print_la_SOURCES = $(sources)
mca_db_print_la_LDFLAGS = -module -avoid-version $(db_print_LDFLAGS)
mca_db_print_la_LIBADD = $(db_print_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_print_la_CPPFLAGS = $(db_print_CPPFLAGS)
libmca_db_print_la_SOURCES =$(sources)
libmca_db_print_la_LDFLAGS = -module -avoid-version $(db_print_LDFLAGS)
libmca_db_print_la_LIBADD = $(db_print_LIBS)
