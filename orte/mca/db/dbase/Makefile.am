#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        db_dbase.h \
        db_dbase_component.c \
        db_dbase.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_db_dbase_DSO
component_noinst =
component_install = mca_db_dbase.la
else
component_noinst = libmca_db_dbase.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_dbase_la_CPPFLAGS = $(db_dbase_CPPFLAGS)
mca_db_dbase_la_SOURCES = $(sources)
mca_db_dbase_la_LDFLAGS = -module -avoid-version $(db_dbase_LDFLAGS)
mca_db_dbase_la_LIBADD = $(db_dbase_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_dbase_la_CPPFLAGS = $(db_dbase_CPPFLAGS)
libmca_db_dbase_la_SOURCES =$(sources)
libmca_db_dbase_la_LDFLAGS = -module -avoid-version $(db_dbase_LDFLAGS)
libmca_db_dbase_la_LIBADD = $(db_dbase_LIBS)
