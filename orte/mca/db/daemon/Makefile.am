#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        db_daemon.h \
        db_daemon_component.c \
        db_daemon.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_db_daemon_DSO
component_noinst =
component_install = mca_db_daemon.la
else
component_noinst = libmca_db_daemon.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_db_daemon_la_CPPFLAGS = $(db_daemon_CPPFLAGS)
mca_db_daemon_la_SOURCES = $(sources)
mca_db_daemon_la_LDFLAGS = -module -avoid-version $(db_daemon_LDFLAGS)
mca_db_daemon_la_LIBADD = $(db_daemon_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_db_daemon_la_CPPFLAGS = $(db_daemon_CPPFLAGS)
libmca_db_daemon_la_SOURCES =$(sources)
libmca_db_daemon_la_LDFLAGS = -module -avoid-version $(db_daemon_LDFLAGS)
libmca_db_daemon_la_LIBADD = $(db_daemon_LIBS)
