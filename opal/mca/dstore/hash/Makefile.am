#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
# Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        dstore_hash.h \
        dstore_hash_component.c \
        dstore_hash.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_dstore_hash_DSO
component_noinst =
component_install = mca_dstore_hash.la
else
component_noinst = libmca_dstore_hash.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_dstore_hash_la_SOURCES = $(sources)
mca_dstore_hash_la_LDFLAGS = -module -avoid-version
mca_dstore_hash_la_LIBADD = $(dstore_hash_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_dstore_hash_la_SOURCES =$(sources)
libmca_dstore_hash_la_LDFLAGS = -module -avoid-version
