# -*- shell-script -*-
#  
#
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
#  Additional copyrights may follow
# 
# $HEADER$
# 
# 
AM_CPPFLAGS = $(OSHMEM_CFLAGS) $(coll_fca_CPPFLAGS) -DCOLL_FCA_HOME=\"$(coll_fca_HOME)\" -I$(coll_fca_HOME)/include/fca -I$(coll_fca_HOME)/include/fca_core 
scoll_fca_sources = \
					scoll_fca.h \
					scoll_fca_debug.h \
					scoll_fca_api.h \
					scoll_fca_module.c \
					scoll_fca_component.c \
					scoll_fca_ops.c
if MCA_BUILD_oshmem_scoll_fca_DSO
component_noinst =
component_install = mca_scoll_fca.la
else
component_noinst = libmca_scoll_fca.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_scoll_fca_la_SOURCES = $(scoll_fca_sources)
mca_scoll_fca_la_LIBADD = $(scoll_fca_LIBS)
mca_scoll_fca_la_LDFLAGS = -module -avoid-version $(scoll_fca_LDFLAGS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_scoll_fca_la_SOURCES =$(scoll_fca_sources)
libmca_scoll_fca_la_LIBADD = $(scoll_fca_LIBS)
libmca_scoll_fca_la_LDFLAGS = -module -avoid-version $(scoll_fca_LDFLAGS)
