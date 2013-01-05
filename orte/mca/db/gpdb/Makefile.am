#
# Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        db_gpdb.h \
        db_gpdb_component.c \
        db_gpdb.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_db_gpdb_DSO
component_noinst =
component_install = mca_db_gpdb.la
else
component_noinst = libmca_db_gpdb.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_gpdb_la_CPPFLAGS = $(db_gpdb_CPPFLAGS)
mca_db_gpdb_la_SOURCES = $(sources)
mca_db_gpdb_la_LDFLAGS = -module -avoid-version $(db_gpdb_LDFLAGS)
mca_db_gpdb_la_LIBADD = $(db_gpdb_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_gpdb_la_CPPFLAGS = $(db_gpdb_CPPFLAGS)
libmca_db_gpdb_la_SOURCES =$(sources)
libmca_db_gpdb_la_LDFLAGS = -module -avoid-version $(db_gpdb_LDFLAGS)
libmca_db_gpdb_la_LIBADD = $(db_gpdb_LIBS)
