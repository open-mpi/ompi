#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        db_dbm.h \
        db_dbm_component.c \
        db_dbm.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_db_dbm_DSO
component_noinst =
component_install = mca_db_dbm.la
else
component_noinst = libmca_db_dbm.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_dbm_la_CPPFLAGS = $(db_dbm_CPPFLAGS)
mca_db_dbm_la_SOURCES = $(sources)
mca_db_dbm_la_LDFLAGS = -module -avoid-version $(db_dbm_LDFLAGS)
mca_db_dbm_la_LIBADD = $(db_dbm_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_dbm_la_CPPFLAGS = $(db_dbm_CPPFLAGS)
libmca_db_dbm_la_SOURCES =$(sources)
libmca_db_dbm_la_LDFLAGS = -module -avoid-version $(db_dbm_LDFLAGS)
libmca_db_dbm_la_LIBADD = $(db_dbm_LIBS)
