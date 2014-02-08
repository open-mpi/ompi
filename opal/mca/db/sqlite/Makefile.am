#
# Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

dist_ompidata_DATA = help-db-sqlite.txt

sources = \
        db_sqlite.h \
        db_sqlite_component.c \
        db_sqlite.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_db_sqlite_DSO
component_noinst =
component_install = mca_db_sqlite.la
else
component_noinst = libmca_db_sqlite.la
component_install =
endif

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_sqlite_la_CPPFLAGS = $(db_sqlite_CPPFLAGS)
mca_db_sqlite_la_SOURCES = $(sources)
mca_db_sqlite_la_LDFLAGS = -module -avoid-version $(db_sqlite_LDFLAGS)
mca_db_sqlite_la_LIBADD = $(db_sqlite_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_sqlite_la_CPPFLAGS = $(db_sqlite_CPPFLAGS)
libmca_db_sqlite_la_SOURCES =$(sources)
libmca_db_sqlite_la_LDFLAGS = -module -avoid-version $(db_sqlite_LDFLAGS)
libmca_db_sqlite_la_LIBADD = $(db_sqlite_LIBS)
